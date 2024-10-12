#' sim_measures_from_yaml
#' Read similarity measur definitions from a YAML and construct a list of SimilaryMeasure objects
#' @param fname (character) Filename
#'
#' @return list of SimilarityMeasure objects
#' @export
#'
#' @examples
sim_measures_from_yaml <-  function(fname) {
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
      cache = TRUE
    )
  })
}

edit_dist <- function(s, t) {
  utils::adist(s,t)[1,1]
}

edit_sim <- function(s, t) {
  1 - edit_dist(s, t)/max(nchar(s), nchar(t))
}

edit_dist_utf8 <- function(s, t) {
  offset <- min(c(s, t))
  s <- s -  offset + 128
  t <- t -  offset  + 128
  utils::adist(intToUtf8(s),intToUtf8(t))[1,1]
}

utf8_convert <- function(s, t) {
  s <- na.omit(s)
  t <- na.omit(t)
  s <- s %>% as.factor() %>% as.integer()
  t <- t %>% as.factor() %>% as.integer()
  offset <- min(c(s, t))
  s <- s -  offset + 128
  t <- t -  offset  + 128
  list(s = intToUtf8(s),
       t = intToUtf8(t))
}

diss_NCD <- function(s, t, method = "gz") {
  s <- as.character(na.omit(s))
  t <- as.character(na.omit(t))
  #browser()
  if(all(s == t)) return(0)
  st <- as.character(na.omit(sprintf("%s%s", s, t)))
  s_c <-   lapply(s, charToRaw) %>% unlist() %>% memCompress(method) %>% length()
  t_c <-   lapply(t, charToRaw) %>% unlist() %>% memCompress(method) %>% length()
  st_c <-   lapply(st, charToRaw) %>% unlist() %>% memCompress(method) %>% length()
  #browser()
  # d_NCD <- TSclust::diss.NCD(lapply(s, charToRaw) %>% unlist()  %>% as.numeric(),
  #                            lapply(t, charToRaw) %>% unlist()  %>% as.numeric(),
  #                            type = method)
  #d_NCD <- TSclust::diss.NCD(s, t)
  #comp <- TSclust:::.compression.lengths(s, t, method)
  #d_NCD <- (comp$cxy - min(comp$cx, comp$cy))/max(comp$cx, comp$cy)
  d_ncd <- (st_c - min(s_c, t_c))/max(s_c, t_c)
  d_ncd
}

sim_NCD <- function(s, t, method = "gz") {
  #browser()
  d <- diss_NCD(s, t, method)
  1 - d
}

sim_emd <- function(mel1, mel2, beta = 1) {
  emdist::emd(mel1 %>% select(duration, onset, pitch) %>% as.matrix(),
              mel2 %>% select(duration, onset, pitch) %>% as.matrix()) %>% (function(x){ exp(-beta *x) })
}

sim_dtw <- function(mel1, mel2, beta = 1) {
  dist <- dtw::dtw(mel1$onset, mel2$onset)
  if(is.null(beta) || !is.numeric(beta) || beta <= 0) {
    #print(dist$normalizedDistance)
    return(exp( - 2*dist$normalizedDistance))
  }
  dist$normalizedDistance %>% (function(x){exp(-beta *x)})
}

compression_ratio <- function(s, t, method = "gz") {
  s <- as.character(na.omit(s))
  t <- as.character(na.omit(t))
  st <- as.character(na.omit(sprintf("%s%s", s, t)))
  s_r <- lapply(s, charToRaw) %>% unlist() %>% length()
  t_r <- lapply(t, charToRaw) %>% unlist() %>% length()
  st_r <- lapply(st, charToRaw) %>% unlist() %>% length()
  s_c <-   lapply(s, charToRaw) %>% unlist() %>% memCompress(method) %>% length()
  t_c <-   lapply(t, charToRaw) %>% unlist() %>% memCompress(method) %>% length()
  st_c <-   lapply(st, charToRaw) %>% unlist() %>% memCompress(method) %>% length()
  overhead <- length(memCompress(raw(0), method))
  #browser()
  comp_r_raw <- (s_c - overhead + t_c - overhead)/(s_r + t_r)
  comp_r_cat <- (st_c - offset)/st_r
  logging::loginfo(sprintf("Raw %.3f, combined: %.3f, var1: %.3f", comp_r_raw, comp_r_cat, (st_c - overhead)/(s_c - offset + t_c - overhead)))
  return(2*(1-comp_r_cat/comp_r_raw))
}

na.omit <- function(x) {
  x[!is.na(x)]
}

edit_sim_utf8 <- function(s, t) {
  if(!is.numeric(s) || !is.numeric(t)){
    logging::logerror("Called edit_sim_utf8 with non-numeric vectors")
    stop()
  }
  s <- s[!is.na(s)]
  t <- t[!is.na(t)]

  offset <- min(c(s, t))
  s <- s -  offset + 256
  t <- t -  offset  + 256
  1 - utils::adist(intToUtf8(s),intToUtf8(t))[1,1]/max(length(s), length(t))
}

stringdot_utf8 <- function(s, t, length = 4, method = "spectrum") {
  if(!is.numeric(s) || !is.numeric(t)){
    logging::logerror("Called stringdot_utf8 with non-numeric vectors")
    stop()
  }
  s <- s[!is.na(s)]
  t <- t[!is.na(t)]

  offset <- min(c(s, t))
  s <- s -  offset + 256
  t <- t -  offset  + 256
  sk <- kernlab::stringdot(length, method, normalized = T)
  sk(intToUtf8(s),intToUtf8(t))
}

make_stringdot_utf8 <- function(parameters = list(ngram_length = 4, method = "spectrum")){
  logging::loginfo(sprintf("Made stringdot measure with %d parameters", length(parameters)))
  if(is.null(parameters)){
    return(stringdot_utf8)
  }
  length <- 4
  if(!is.null(parameters$ngram_length)){
    length <- parameters$ngram_length
  }
  method <- "spectrum"
  if(!is.null(parameters$method)){
    method <- parameters$method
  }
  function(s, t){
    stringdot_utf8(s, t, length, method)
  }
}
dist_sim <- function(x, y){
  if(length(x) == 0 || length(y) == 0){
    return(0)
  }
  joint <- c(x, y) %>% table()
  tx <- factor(x, levels = names(joint)) %>% table()
  ty <- factor(y, levels = names(joint)) %>% table()
  joint <- joint/sum(joint)
  tx <- tx/sum(tx)
  ty <- ty/sum(ty)
  tv <- 1 - .5 * sum(abs(tx - ty))
  if(tv < 0 || tv > 1){
  }
  tv
}

ukkon <- function (x, y, na.rm = TRUE) {
  if(na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  }
  joint <- c(x, y) %>% table()
  tx <- factor(x, levels = names(joint)) %>% table()
  ty <- factor(y, levels = names(joint)) %>% table()
  1 - sum(abs(tx - ty))/(length(x) + length(y))
}

count_distinct <- function(x, y, na.rm = TRUE) {
  if(na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  }
  length(intersect(x, y))/max(length(x), length(y))
}

sum_common <- function(x, y, na.rm = TRUE) {
  if(na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  }
  joint <- c(x, y) %>% table()
  tx <- factor(x, levels = names(joint)) %>% table()
  ty <- factor(y, levels = names(joint)) %>% table()
  is <- intersect(names(tx[tx > 0]), names(ty[ty > 0]))
  sum(tx[is] + ty[is])/(length(x) + length(y))
}

proxy_pkg_handler <- function(x, y, proxy_method = "Jaccard", na.rm = TRUE, rescale_independently = TRUE, ...) {

  if(!proxy::pr_DB$entry_exists(proxy_method)) {
    logging::logwarn(sprintf("Method %s does not exist in proxy::pr_DB", proxy_method))
  }

  if((is.character(x) || is.character(y)) && proxy_method != "Levenshtein") {
    x <- as.factor(x)
    y <- as.factor(y)
  }

  # We do this in two steps so we can cover the different use cases

  if(is.factor(x) || is.factor(y)) {
    x <- as.integer(x)
    y <- as.integer(y)
  }


  if(na.rm) {
    x <- na.omit(x)
    y <- na.omit(y)
  }

  proxy_type <- get_proxy_sim_measure_type(proxy_method)


  if(proxy_method == "fJaccard") {
    rescaled_vs <- rescale_vectors(x, y, rescale_independently)
    x <- rescaled_vs$x_rescaled
    y <- rescaled_vs$y_rescaled
  }


  if(proxy_method == "Levenshtein") {

    stopifnot(is.character(x) && is.character(y))

    res <- proxy::dist(x, y, method = "Levenshtein", ...) %>%
      as.numeric()

    return(res)

  } else if(proxy_type == "binary") {
    xy <- union(x, y)
    input <- rbind(xy %in%  x, xy %in%  y)

  } else if(proxy_type %in% c("metric", "nominal", "mixed")) {
    if(length(x) != length(y)){
      browser()
      logging::logwarn(sprintf("Length of vectors not identicial for proxy method: %s", proxy_method))
    }
    input <- rbind(x, y)

  } else {
    stop("proxy_type not recognised.")
  }

  if(is_distance_measure(proxy_method)) {
    res <- proxy::dist(input, method = proxy_method, ...) %>%
      proxy::pr_dist2simil()
  } else {
    res <- proxy::simil(input, method = proxy_method, ...)
  }
  if(res < 0 || res > 1){
    browser()
  }
  res %>%
    as.numeric()


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

file_ext <- function(file) {
  tmp <- str_extract(file, "\\.[a-zA-Z0-9]+$")
  ifelse(length(tmp), str_sub(tmp, 2), "")
}

create_corpus_from_csvs <- function(f) {
  list.files(f, pattern = "csv", full.names = TRUE) %>%
    purrr::map(read_melody)
}

read_melody <- function(f) {
  melody_factory$new(fname = f, name = tools::file_path_sans_ext(basename(f)))
}

#' update_melodies
#' Updates a list of melody objects to the current version of the melody object (as R6 objects keep their bindings, once created)
#' @param mel_list list of Melody objects
#' @param force (logical) Do it no matter what, otherwise only if version of melsim has changed
#'@export
update_melodies <- function(mel_list, force = TRUE){
  current_version <- melody_factory$new()$version
  map(mel_list, function(x){
    if(force || is.null(x$version) || x$version != current_version){
      melody_factory$new(mel_data = x$data, mel_meta = x$meta, override = FALSE)
    }
    else{
      x
    }
  })
}

#' update_sim_mat
#' Updates a SimilarityMatrix object
#' @param sim_mat SimilarityMatrix object
#' @param force (logical) Do it no matter what, otherwise only if version of melsim has changed
#'
#'@export
update_sim_mat <- function(sim_mat, force = TRUE) {
  ret <- sim_mat_factory$new(sim_mat$data, paired = sim_mat$mat_type == "paired")
  current_version <- ret$version
  if(force || is.null(sim_mat$version) || sim_mat$version != current_version){
    ret
  } else{
    sim_mat
  }
}

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
           round(stats::median(pitch_vec1)) - round(stats::median(pitch_vec2)))
  octave_offset <- modus(round(ret/12))
  #logging::loginfo(sprintf("Octave offset = %d", octave_offset))
  ret <- c(0, ret, octave_offset * 12 + key_diff, octave_offset * 12 + 12 - key_diff)
  unique(ret) %>% sort()

}
# Finds transposition that maximize raw edit distance of two pitch vectors
# Transposition in semitone of the *second* melody
find_best_transposition <- function(pitch_vec1, pitch_vec2){
  trans_hints <- get_transposition_hints(pitch_vec1, pitch_vec2)
  sims <- purrr::map_dfr(trans_hints, function(x) {
    tidyr::tibble(transposition = x,
                  dist = edit_dist(intToUtf8(pitch_vec1), intToUtf8(pitch_vec2 + x)))
  })
  sims %>% dplyr::arrange(dist, abs(transposition)) %>% utils::head(1) %>% dplyr::pull(transposition)
}



#' Get harmonies via the Krumhansl-Schmuckler algorithm
#'
#' @param pitch_vec (integer) Pitch vector
#' @param segmentation (factor) Segmentation as grouping variable
#' @param only_winner (logical) Return only winning harmony
#' @param fast_algorithm (logical) Use, fast matrix-base algorithm
#'
#' @return
#' @export
#'
#' @examples
get_implicit_harmonies <- function(pitch_vec, segmentation = NULL, weights = NULL, only_winner = TRUE, fast_algorithm = TRUE) {

  # warning('Segmentation format must be as segment ID')
  # Krumhansl-Schmuckler algorithm
  ks_weights_major <- c(6.33, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  ks_weights_minor <- c(6.35, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)

  ks_weights_major_z <- scale(ks_weights_major) %>% as.numeric()
  ks_weights_minor_z <- scale(ks_weights_minor) %>% as.numeric()

  ks_weights_major_mat <- sapply(0:11, function(t) ks_weights_major_z[((0:11 - t) %% 12) + 1])
  ks_weights_minor_mat <- sapply(0:11, function(t) ks_weights_minor_z[((0:11 - t) %% 12) + 1])
  ks_mat <- cbind(ks_weights_major_mat, ks_weights_minor_mat)

  if(!is.null(segmentation)) {
    if(length(segmentation) != length(pitch_vec)) {
      stop("Segmentation must be of same length as pitch")
    }
    s <- unique(segmentation)
    if(fast_algorithm) {
      pitch_vec <- factor(pitch_vec %% 12, levels = 0:11)
      pitch_freq_vec <- sapply(s, function(x) table(pitch_vec[segmentation == x]) %>% scale() %>% as.numeric())
      ks_cor <- t(pitch_freq_vec) %*% ks_mat
      ret <-
        purrr::imap_dfr(s, function(seg, i) {
          if(only_winner) {
            winner <- ks_cor[i, ] %>% which.max()
            tibble(segment = seg,
                   key_pc = (winner - 1) %% 12,
                   type = ifelse(winner <= 12, "major", "minor"),
                   key = sprintf("%s-%s",
                                 pc_labels_flat[key_pc + 1], substr(type, 1, 3)),
                   match = ks_cor[i, winner] %>% as.numeric()
                   )
          } else{
            tibble(
              segment = seg,
              key_pc = rep(0:11, 2),
              type = rep(c("major", "minor"), each = 12),
              key = sprintf("%s-%s",
                            pc_labels_flat[key_pc + 1], substr(type, 1, 3)),
              match = ks_cor[i,] %>% as.numeric()) %>%
              dplyr::arrange(segment, desc(match))
          }
        })
      return(ret)
    }
    return(
      purrr::map_dfr(s, function(x) {
        pv <- pitch_vec[segmentation == x]
        tidyr::tibble(segment = x) %>%
          get_implicit_harmonies(pv, NULL,
                                 only_winner = only_winner,
                                 fast_algorithm = fast_algorithm,
                                 weights = weights) %>% bind_cols()
      })
    )

  }
  if(!is.null(weights)) {
    if(length(weights) != length(pitch_vec)) {
      logging::logerror("Weights must have same length as pitches")
      stop()
    }
    tab <- table(factor(pitch_vec %% 12, levels = 0:11), weights)
    pitch_freq <- tab  %*% as.numeric(colnames(tab))  %>% scale() %>% as.numeric()
  } else{
    pitch_freq <- table(factor(pitch_vec %% 12, levels = 0:11)) %>% scale() %>% as.numeric()

  }
  if(fast_algorithm) {
    ks_cor <- pitch_freq %*% ks_mat
    correlations <- tibble(key_pc = rep(0:11, 2),
                           type = rep(c("major", "minor"), each = 12),
                           key = sprintf("%s-%s",
                                         pc_labels_flat[key_pc + 1], substr(type, 1,3)),
                           match = ks_cor %>% as.numeric()) %>%
      dplyr::arrange(desc(match))
  }
  else {
    correlations <- purrr::map_dfr(0:11, function(t) {
      w_major <- stats::cor.test(pitch_freq,
                          ks_weights_major_mat[, t + 1])$estimate
      w_minor <- stats::cor.test(pitch_freq,
                          ks_weights_minor_mat[, t + 1])$estimate
      dplyr::bind_rows(tidyr::tibble(key_pc = t,
                                     match = w_major,
                                     type = "major",
                                     key = sprintf("%s-maj", pc_labels_flat[t + 1])),
                       tidyr::tibble(key_pc = t,
                                     match = w_minor,
                                     type = "minor",
                                     key = sprintf("%s-min", pc_labels_flat[t + 1])))
    }) %>%
      dplyr::arrange(desc(match))
  }
  if(only_winner) {
    return(correlations[1,])
  }
  correlations
}

bootstrap_implicit_harmonies <- function(pitch_vec, segmentation = NULL, sample_frac = .8, size = 10) {
  if(!is.null(segmentation)) {
    segments <- unique(segmentation)
    ret <-
      purrr::map_dfr(segments, function(seg) {
        bootstrap_implicit_harmonies(pitch_vec[segmentation == seg],
                                     NULL,
                                     sample_frac = sample_frac,
                                     size = size) %>%
          dplyr::mutate(segment = seg)
      })
    return(ret)
  }
  l <- length(pitch_vec)
  sample_size <- max(1, round(sample_frac * l))

  bs <-
    purrr::map_dfr(1:size, function(x) {
      pv <- sample(pitch_vec, replace = TRUE, sample_size)
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

entropy_wrapper <- function(vec, norm = FALSE, domain = NULL, na.rm = TRUE) {
  norm_factor <- 1
  if(na.rm) {
    vec <- vec[!is.na(vec)]
  }
  if(norm){
    if(is.null(domain)){
      domain <- vec
    }
    norm_factor <- 1/log2(length(unique(domain)))
  }
  norm_factor * entropy::entropy(table(vec), unit = "log2")
}

interval_difficulty <- function(int_vec, na.rm = TRUE) {

  if(is.list(int_vec)) {
    return(purrr::map_dfr(int_vec, interval_difficulty))
  }

  if(na.rm) {
    int_vec <- int_vec[!is.na(int_vec)]
  }

  if(length(int_vec) <= 1) {
    return(tidyr::tibble(mean_abs_int = NA,
                         int_range = NA,
                         mean_dir_change = NA,
                         int_variety = NA,
                         pitch_variety = NA,
                         mean_run_length = NA))
  }

  mean_abs_int <- mean(abs(int_vec))

  int_range <- max(abs(int_vec))

  l <- length(int_vec)
  r <- rle(sign(int_vec))
  dir_change <- length(r$values) - 1
  mean_dir_change <- (length(r$values) - 1)/(l - 1)
  mean_run_length <- 1 - mean(r$lengths)/l

  int_variety <- dplyr::n_distinct(int_vec)/l
  pitch_variety <- dplyr::n_distinct(c(0, cumsum(int_vec)))/(l + 1)

  res <- tidyr::tibble(mean_abs_int = mean_abs_int,
                       int_range = int_range,
                       mean_dir_change = mean_dir_change,
                       int_variety = int_variety,
                       pitch_variety = pitch_variety,
                       mean_run_length = mean_run_length)
  res$mean_dir_change[!is.finite(res$mean_dir_change)] <- NA
  res
}

get_tonal_features <- function(implicit_harmonies) {
  if(!is.null(implicit_harmonies[["segment"]])) {
    ret <- map_dfr(unique(implicit_harmonies[["segment"]]), function(seg) {
      get_tonal_features(implicit_harmonies %>% filter(segment == seg) %>% select(-segment)) %>%
        mutate(segment = seg)
    })
    return(ret)
  }
  implicit_harmonies <- implicit_harmonies %>% arrange(desc(match))
  A0 <- implicit_harmonies$match[1]
  A1 <- implicit_harmonies$match[2]
  tonal_spike <- A0 / sum(implicit_harmonies$match[implicit_harmonies$match > 0])
  mode <- str_split_fixed(implicit_harmonies %>% slice(1) %>% pull(key), "-", 2)[,2]

  tibble(tonalness = A0,
         tonal_clarity = A0/A1,
         tonal_spike,
         mode)


}

