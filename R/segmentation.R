segment_ids_to_marker <- function(segment_ids, end = F){
  markers <- segment_ids  %>% diff()
  #browser()
  if(!all(markers %in% c(0, 1))){
    logging::logerror("Segment IDs must be consecutive")
  }
  if(!end){
    markers <- c(1, markers)
  }
  else{
    markers <- c(markers, 1)
  }
  markers
}

fuse_single_note_motifs <- function(pos){
  starts <- which(pos == 1)
  singles <- which(diff(starts) == 1)
  if(length(singles) == 0){
    return(pos)
  }
  pos[starts[singles] + 1] <- 0
  return(pos)
}

fuse_two_note_motifs <- function(mel_obj, pos){
  #browser()
  mel <- mel_obj$data
  segment_ids <- cumsum(pos)
  starts <- which(pos == 1)
  two_tones <- which(diff(starts) == 2)
  if(length(two_tones) == 0){
    return(pos)
  }
  for(i in starts[two_tones]){
    tmp <- mel %>% slice(i, i + +2)
    pre_ioi <- -Inf
    post_ioi <- +Inf
    pre_ioi_class <- 2
    post_ioi_class <- 2

    if(i >  1){
      pre_ioi <- mel %>% slice(i - 1) %>% pull(ioi)
      pre_ioi_class <- mel %>% slice(i - 1) %>% pull(ioi_class)
    }
    else{
      offset <- 2
    }

    if (i + 1 < length(pos)){
      post_ioi <- mel %>% slice(i + 1) %>% pull(ioi)
      post_ioi_class <- mel %>% slice(i + 1) %>% pull(ioi_class)
    }
    else{
      offset <- 0
    }

    if(pre_ioi >= post_ioi){
      offset <- 0
    }
    else if(pre_ioi < post_ioi){
      offset <- 2
    }
    else{
      #browser()
      next
    }
    if(offset == 2 & post_ioi >= 0){
      offset <- 0
    }
    if(offset == 0 & pre_ioi_class > 0){
      #messagef("Skipping...")
      next
    }
    #logging::loginfo("Fusing two tone motifs @%d (i = %d, offset = %d)", i + offset, i, offset)
    pos[i + offset] <- 0
  }
  #browser()
  pos
}
#' New segmentation function, does a rather fine segmentation, in fact, it looks for motifs not segments
#' On the Kinder set, the algorithm receives an median F1 of .65, whereas
#' itembankr::get_segmentation achvievs a median F1 of .38
#' @export
motifator <- function(mel_obj, threshold = 3){
  mel <- mel_obj$data
  #browser()
  if(!("ioi_class") %in% names(mel_obj)){
    mel$ioi_class <- melsim::classify_duration(mel$ioi, ref_duration = unique(mel$beat_duration)[1])
  }
  d_ioi <- sign(diff(mel$ioi))
  pos_ioi <- rep(0, length(na.omit(mel$ioi)))
  pos_ioi[d_ioi < 0] <- 1

  d_ioi_c <- sign(diff(mel$ioi_class))
  pos_ioi_c <- rep(0, length(na.omit(mel$ioi)))
  pos_ioi_c[d_ioi_c < 0] <- 1

  pos_abs <- rep(0, length(na.omit(mel$ioi)))
  pos_abs[na.omit(mel$ioi_class) >= 0] <- 1

  pos <- rep(0, length(na.omit(mel$ioi)))
  values <- na.omit(pos_abs + pos_ioi + pos_ioi_c)
  pos[values >= threshold] <- 1

  pos <- c(1, pos)
  tmp <- pos
  while(T){
    new_tmp <- fuse_single_note_motifs(tmp)
    if(all(new_tmp == tmp)){
      pos <- tmp
      break
    }
    else{
      tmp <- new_tmp
    }
    #logging::loginfo("Fusing singles...")

  }
  pos <- fuse_two_note_motifs(mel_obj, pos)
  #browser()
  segment_ids <- cumsum(pos)
  return(segment_ids)
}

motif_plot <- function(mel_obj, max_motif = Inf, with_facets = T, external = NULL){
  if(is.null(external)){
    tmp <- mel_obj$data %>%
      mutate(motifs = motifator(mel_obj, 3)) %>%
      filter(motifs <= max_motif)
  }
  else{
    if(is.character(external) & external %in% names(mel_obj$data)){
      tmp <- mel_obj$data %>%
        mutate(motifs = !!sym(external)) %>%
        filter(motifs <= max_motif)

    }
  }
  q <- tmp %>%
    ggplot2::ggplot(ggplot2::aes(x = onset, y = pitch, color = factor(motifs)))
  q <- q + ggplot2::geom_point()
  q <- q + ggplot2::geom_line()
  q <- q + ggplot2::ggtitle(mel_obj$meta$name)

  if(with_facets) q <- q + ggplot2::facet_wrap(~motifs, scale = "free_x")
  q <- q + ggplot2::theme_classic()
  q
}

get_motif_augemented_transformation <- function(mel_obj,
                                                transform = "int_X_ioi_class",
                                                symbols = c("start" = "^", "in" = "", "end" = "$"),
                                                as_df = F){
  mel <- mel_obj$data
  logging::loginfo("Augmenting: %s", mel_obj$meta$name)
  motifs <- motifator(mel_obj)
  t <- mel_obj$data[[transform]] %>% na.omit()
  lt <- length(t)
  stopifnot(!is.null(t))
  motifs <- motifs[1:lt]
  end_pos <- c(which(diff(motifs) > 0), lt)
  start_pos <- c(1, which(diff(motifs) > 0) + 1)
  syms <- rep(symbols["in"], lt)
  syms[start_pos] <- symbols["start"]
  syms[end_pos] <- symbols["end"]
  ret <- sprintf("%s%s", t, syms)
  #browser()
  if(as_df){
    l_diff <- nrow(mel) - lt
    ret <- c(ret, rep(NA, l_diff))
    ret <- tibble(id = mel_obj$meta$name, pos = 1:nrow(mel), value = ret)

  }
  ret
}
