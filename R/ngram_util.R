#' Add ngrams of multiple sizes to given melody data frame
#'
#' @param mel_data Melody data frame
#' @params columns (character) Columns names to compute ngrams from
#' @param N (vector of positive integers) N-gram lengths
#' @param override (logical) Whether to override existing ngram columns
#' @return melody data frame with added n
#' @export
#'
#' @examples

add_ngrams <- function(mel_data, columns, N, override = FALSE) {
  ngram_cols <-
    map_dfc(columns, function(col){
      if(!(col %in% names(mel_data))){
        return(NULL)
      }
      ngrams <- get_all_ngrams(mel_data[[col]],
                               N,
                               keep_length = TRUE) %>%
        pivot_wider(id_cols = start,
                    names_from = N,
                    names_prefix = sprintf("%s_ngram_", col),
                    values_from = value) %>%
        select(-start)
      ngrams
    })
  if(!override){
    new_cols <- setdiff(names(ngram_cols), names(mel_data))
    bind_cols(mel_data, ngram_cols %>% select(new_cols))
  }
  else{
    new_cols <- names(ngram_cols)
    bind_cols(mel_data[, setdiff(names(mel_data), new_cols)], ngram_cols)
  }
}

#' Get all ngrams from a given vector
#'
#' @param x Arbitraty Vector
#' @param N (set of positve integers) N.gram lengths
#' @param collapse (string) Glue string for ngrams string representation
#' @param keep_length (logical) Shall the length of the original vector be kept by including NA at the end?
#' @return Data frame of ngrams with N columns
#' @export
#'
#' @examples
get_all_ngrams <- function(x, N = 3, collapse = ",", keep_length = FALSE){
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


