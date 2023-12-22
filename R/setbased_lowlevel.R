


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
get_all_ngrams <- function(x, N = 3, collapse = ",", keep_length = F){
  stopifnot(is.vector(x) & !is.list(x))
  ret <-
    map_dfr(N, function(n){
    if(keep_length){
      l <- length(x)
    }
    else{
      l <- length(x) - n + 1
    }
    if(l <= 0){
      return(NULL)
    }
    purrr::map_dfr(1:l, function(i){
      tibble::tibble(start = i, N = n, value = paste(x[i:(i + n - 1)], collapse = collapse))
    })

  })
  if(keep_length){
    ret[stringr::str_detect(ret$value, "NA"),]$value <- NA
  }
  ret
}


