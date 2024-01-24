#' sim_measures_from_yaml
#' Read similarity measur definitions from a YAML and construct a list of SimilaryMeasure objects
#' @param fname (character) Filename
#'
#' @return list of SimilarityMeasure objects
#' @export
#'
#' @examples
sim_measures_from_yaml <-  function(fname){
  all <- yaml::read_yaml(fname)
  if(is.null(all)){
    logging::logerror(sprintf("%s not a YAML file", fname))
  }
  map(names(all), function(sim_id){
    sm <- all[[sim_id]]
    sim_measure_factory$new(
      name = sim_id,
      full_name = sm$full_name,
      type = sm$type,
      transformation = sm$transformation,
      parameters = sm$parameters,
      sim_measure = sm$sim_measure,
      transposition_invariant = sm$transposition_invariant,
      tempo_invariant = sm$tempo_invariant,
      cache = T
    )
  })
}

edit_dist <- function(s, t){
  adist(s,t)[1,1]
}

edit_sim <- function(s, t){
  1 - edit_dist(s, t)/max(nchar(s), nchar(t))
}

edit_dist_utf8 <- function(s, t){
  adist(intToUtf8(s),intToUtf8(t))[1,1]
}

edit_sim_utf8 <- function(s, t){
  edit_sim(intToUtf8(s),intToUtf8(t))
}

pmi <- function(q, t) {
  q_l <- length(q)
  t_l <- length(t)
  aligned <- Biostrings::pairwiseAlignment(intToUtf8(q),
                                           intToUtf8(t),
                                           type = "global", # i.e., Needleman-Wunsch
                                           gapOpening = 12,
                                           gapExtension = 6)

  q_aligned <- utf8ToInt(as.character(aligned@pattern))
  t_aligned <- utf8ToInt(as.character(aligned@subject))

  sum(q_aligned == t_aligned) / ((q_l + t_l)/2)
}

file_extension <- function(file) strsplit(basename(file), ".", fixed = T)[[1]][-1]

#find a list of candidates for best transpositions for two pitch vectors, based on basic stats
get_transposition_hints <- function(pitch_vec1, pitch_vec2){
  ih1 <- get_implicit_harmonies(pitch_vec1, only_winner = TRUE)
  key1 <- ih1 %>% dplyr::pull(key)
  pc1 <- ih1 %>% dplyr::pull(key_pc)
  ih2 <- get_implicit_harmonies(pitch_vec2, only_winner = TRUE)
  pc2 <- ih2 %>% dplyr::pull(key_pc)
  key_diff <- (pc2 -  pc1) %% 12
  #logging::loginfo(sprintf("Best key 1 = %s, best key 2 = %s, key diff = %d", key1, ih2 %>% head(1) %>% dplyr::pull(key), key_diff ))
  modus1 <- top_n(pitch_vec1, 3)
  modus2 <- top_n(pitch_vec2, 3)
  ret <- c(expand.grid(modus1, modus2) %>%
             mutate(d = Var1 - Var2) %>% pull(d) %>% top_n(3),
           round(mean(pitch_vec1)) - round(mean(pitch_vec2)),
           round(median(pitch_vec1)) - round(median(pitch_vec2)))
  octave_offset <- modus(round(ret/12))
  #logging::loginfo(sprintf("Octave offset = %d", octave_offset))
  ret <- c(0, ret, octave_offset * 12 + key_diff, octave_offset * 12 + 12 - key_diff)
  unique(ret) %>% sort()

}
#finds transposition that maximize raw edit distance of two pitch vectors
#transposition in semitone of the *second* melody
find_best_transposition <- function(pitch_vec1, pitch_vec2){
  trans_hints <- get_transposition_hints(pitch_vec1, pitch_vec2)
  sims <- purrr::map_dfr(trans_hints, function(x){
    tidyr::tibble(transposition = x,
                  dist = edit_dist(intToUtf8(pitch_vec1), intToUtf8(pitch_vec2 + x)))
  })
  sims %>% dplyr::arrange(dist, abs(transposition)) %>% head(1) %>% dplyr::pull(transposition)
}

optim_transposer <- function(query, target,
                             sim_measure = edit_sim_utf8,
                             strategy = c("all", "hints", "best"),
                             ...){
    strategy <- match.arg(strategy)
    # Run for all transpositions and pick the top
    strategy <- match.arg(strategy)

    #query <- as.integer(query - median(query))
    #target <- as.integer(target - median(target))
    query <- query + 60 - min(c(query))
    target <- target + 60 - min(c(target))
    d <- median(target)  -  median(query)
    if(strategy == "all"){
      hints <- -5:6
    }
    else if(strategy == "hints" ){
      hints <- get_transposition_hints(target, query )
    }
    else if(strategy == "best" ){
      hints <- find_best_transposition(target, query)
    }
    purrr::map_dfr(union(d, hints), function(trans) {
      #browser()
      tibble(trans = trans, sim = sim_measure(query + trans, target))
    }) %>%
      slice_max(sim) %>% distinct(sim) %>% pull(sim)


}

#' get harmonies via the Krumhansl-Schmuckler algorithm
#'
#' @param pitch_vec
#' @param segmentation
#' @param only_winner
#'
#' @return
#' @export
#'
#' @examples
get_implicit_harmonies <- function(pitch_vec, segmentation = NULL, only_winner = TRUE, fast_algorithm = FALSE){
  #warning('Segmentation format must be as segment ID')
  # Krumhansl-Schmuckler algorithm
  ks_weights_major <- c(6.33, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  ks_weights_minor <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)
  ks_weights_major_z <- scale(ks_weights_major) %>% as.numeric()
  ks_weights_minor_z <- scale(ks_weights_minor) %>% as.numeric()

  ks_weights_major_mat <- sapply(0:11, function(t) ks_weights_major_z[((0:11 - t) %% 12) + 1])
  ks_weights_minor_mat <- sapply(0:11, function(t) ks_weights_minor_z[((0:11 - t) %% 12) + 1])
  ks_mat <- cbind(ks_weights_major_mat, ks_weights_minor_mat)

  if(!is.null(segmentation)){
    if(length(segmentation) != length(pitch_vec)){
      stop("Segmentation must be of same length as pitch")
    }
    s <- unique(segmentation)
    if(fast_algorithm){
      pitch_vec <- factor(pitch_vec %% 12, levels = 0:11)
      pitch_freq_vec <- sapply(s, function(x) table(pitch_vec[segmentation == x]) %>% scale() %>% as.numeric())
      ks_cor <- t(pitch_freq_vec) %*% ks_mat
      ret <-
        imap_dfr(s, function(seg, i){
          if(only_winner){
            winner <- ks_cor[i, ] %>% which.max()
            tibble(segment = seg,
                   key_pc = (winner - 1) %% 12,
                   type = ifelse(winner <= 12, "major", "minor"),
                   key = sprintf("%s-%s",
                                 itembankr::pc_labels_flat[key_pc + 1], substr(type, 1, 3)),
                   match = ks_cor[i, winner] %>% as.numeric()
                   )
          }
          else{
            tibble(
              segment = seg,
              key_pc = rep(0:11, 2),
              type = rep(c("major", "minor"), each = 12),
              key = sprintf("%s-%s",
                            itembankr::pc_labels_flat[key_pc + 1], substr(type, 1,3)),
              match = ks_cor[i,] %>% as.numeric()) %>%
              dplyr::arrange(segment, desc(match))
          }
        })
      return(ret)
    }
    return(
      purrr::map_dfr(s, function(x){
        pv <- pitch_vec[segmentation == x]
        tidyr::tibble(segment = x) %>%
          bind_cols(get_implicit_harmonies(pv, NULL,
                                           only_winner = only_winner,
                                           fast_algorithm = fast_algorithm )
                    )
      })
    )

  }
  pitch_freq <- table(factor(pitch_vec %% 12, levels = 0:11)) %>% scale() %>% as.numeric()
  if(fast_algorithm){
    ks_cor <- pitch_freq %*% ks_mat

    correlations <- tibble(key_pc = rep(0:11, 2),
                           type = rep(c("major", "minor"), each = 12),
                           key = sprintf("%s-%s",
                                         itembankr::pc_labels_flat[key_pc + 1], substr(type, 1,3)),
                           match = ks_cor %>% as.numeric()) %>%
      dplyr::arrange(desc(match))
  }
  else{
    correlations <- purrr::map_dfr(0:11, function(t){
      w_major <- cor.test(pitch_freq,
                          ks_weights_major_mat[, t + 1])$estimate
      w_minor <- cor.test(pitch_freq,
                          ks_weights_minor_mat[, t + 1])$estimate
      dplyr::bind_rows(tidyr::tibble(key_pc = t,
                                     match = w_major,
                                     type = "major",
                                     key = sprintf("%s-maj", itembankr::pc_labels_flat[t + 1])),
                       tidyr::tibble(key_pc = t,
                                     match = w_minor,
                                     type = "minor",
                                     key = sprintf("%s-min", itembankr::pc_labels_flat[t + 1])))
    }) %>%
      dplyr::arrange(desc(match))
  }
  if(only_winner){
    return(correlations[1,])
  }
  correlations
}

bootstrap_implicit_harmonies <- function(pitch_vec, segmentation = NULL, sample_frac = .8, size = 10) {
  if(!is.null(segmentation)){
    segments <- unique(segmentation)
    ret <-
      purrr::map_dfr(segments, function(seg){
        bootstrap_implicit_harmonies(pitch_vec[segmentation == seg],
                                     NULL,
                                     sample_frac = sample_frac,
                                     size = size) %>%
          dplyr::mutate(segment = seg)
      })
    return(ret)
  }
  l <-length(pitch_vec)
  sample_size <- max(1, round(sample_frac * l))

  bs <-
    purrr::map_dfr(1:size, function(x){
      pv <- sample(pitch_vec, replace = T, sample_size)
      get_implicit_harmonies(pitch_vec = pv,  only_winner = TRUE)
    })
  best_key <- bs %>% dplyr::count(key) %>% dplyr::arrange(dplyr::desc(n)) %>% dplyr::pull(key)
  bs %>% dplyr::filter(key == best_key[1]) %>% head(1)
}

#' @export
classify_duration <- Vectorize(
  function(dur_vec, ref_duration = .5){
    rel_dur <- dur_vec/ref_duration
    rhythm_class <- rep(NA, length(rel_dur))
    rhythm_class[rel_dur > 0 & !is.na(rel_dur)] <- -2
    rhythm_class[rel_dur > 0.45] <- -1
    rhythm_class[rel_dur > 0.9] <- 0
    rhythm_class[rel_dur > 1.8] <- 1
    rhythm_class[rel_dur > 3.3] <- 2
    rhythm_class
  })

#' @export
fuzzyint_class <- Vectorize(
  function(x){
    if(is.na(x)) return(NA)
    class_vec <- list("0" = 0, "1" = 1, "2" = 1, "3" = 2, "4" = 2, "5" = 3, "6" = 3, "7" = 3)
    s <- sign(x)
    a <- abs(x)
    if(a > 7){
      return(s * 4)
    }
    return(s * class_vec[[as.character(a)]])

  })

