

#find a list of candidates for best transpositions for two pitch vectors, based on basic stats
get_transposition_hints <- function(pitch_vec1, pitch_vec2){
  ih1 <- get_implicit_harmonies(pitch_vec1, only_winner = TRUE)
  key1 <- ih1 %>% dplyr::pull(key)
  pc1 <- ih1 %>% dplyr::pull(transposition)
  ih2 <- get_implicit_harmonies(pitch_vec2, only_winner = TRUE)
  pc2 <- ih2 %>% dplyr::pull(transposition)
  key_diff <- (pc2 -  pc1) %% 12
  #messagef("Best key 1 = %s, best key 2 = %s, key diff = %d", key1, ih2 %>% head(1) %>% dplyr::pull(key), key_diff )
  modus1 <- modus(pitch_vec1)
  modus2 <- modus(pitch_vec2)
  ret <- c(expand.grid(modus1, modus2) %>% mutate(d = Var1 - Var2) %>% pull(d) %>% min(),
           round(mean(pitch_vec1)) - round(mean(pitch_vec2)),
           round(median(pitch_vec1)) - round(median(pitch_vec2)))
  octave_offset <- modus(round(ret/12))
  #messagef("Octave offset = %d", octave_offset)
  ret <- c(0, ret, octave_offset*12 + key_diff, octave_offset * 12 + 12 - key_diff)
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
get_implicit_harmonies <- function(pitch_vec, segmentation = NULL, only_winner = TRUE){

  #warning('Segmentation format must be as segment ID')

  # Krumhansl-Schmuckler algorithm
  ks_weights_major <- c(6.33, 2.23, 3.48, 2.33, 4.38, 4.09, 2.52, 5.19, 2.39, 3.66, 2.29, 2.88)
  ks_weights_minor <- c(6.33, 2.68, 3.52, 5.38, 2.60, 3.53, 2.54, 4.75, 3.98, 2.69, 3.34, 3.17)
  if(!is.null(segmentation)){
    if(length(segmentation) != length(pitch_vec)){
      stop("Segmentation must be of same length as pitch")
    }
    s <- unique(segmentation)
    return(
      purrr::map_dfr(s, function(x){
        pv <- pitch_vec[segmentation == x]
        tidyr::tibble(segment = x,
                      key = get_implicit_harmonies(pv, NULL, only_winner = only_winner) %>%
                        dplyr::pull(key))
      })
    )

  }
  pitch_freq <- table(factor(pitch_vec %% 12, levels = 0:11))
  correlations <- purrr::map_dfr(0:11, function(t){
    w_major <- cor.test(pitch_freq,
                        ks_weights_major[((0:11 - t) %% 12) + 1]) %>%
      broom::tidy() %>%
      dplyr::pull(estimate)
    w_minor <- cor.test(pitch_freq,
                        ks_weights_minor[((0:11 - t) %% 12) + 1]) %>%
      broom::tidy() %>%
      dplyr::pull(estimate)
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
