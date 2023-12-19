


#' Get ngrams of multiple sizes
#'
#' @param abs_melody
#' @param M
#'
#' @return
#' @export
#'
#' @examples
get_ngrams_multiple_sizes <- function(abs_melody, M) {

  if (length(abs_melody) == 1) {
    ngrams_multi <- tibble::tibble(start = NA, N = 1, value = paste(abs_melody, collapse = ","))
  } else if (length(abs_melody) == 2) {
    ngrams_multi <- tibble::tibble(start = NA, N = 2, value = paste(abs_melody, collapse = ","))
  } else {
    if(length(abs_melody) < M) {
      M <- length(abs_melody)
    }

    # Grab all N-grams from 3:M for a given melody
    ngrams_multi <- purrr::map_dfr(3:M, get_all_ngrams, x = abs_melody)
    # But this shouldn't be called if the melody length is shorter than the N-gram length..
  }
  ngrams_multi
}

#' Get all ngrams from a given vector
#'
#' @param x
#' @param N
#'
#' @return
#' @export
#'
#' @examples
get_all_ngrams <- function(x, N = 3){
  l <- length(x) - N + 1
  stopifnot(l > 0)
  purrr::map_dfr(1:l, function(i){
    ngram <- x[i:(i + N - 1)]
    tibble::tibble(start = i, N = N, value = paste(ngram, collapse = ","))
  })
}
