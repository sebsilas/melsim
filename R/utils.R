

is.null.or <- function(x, f) {
  is.null(x) || f(x)
}

is.scalar.character <- function(x) {
  is.character(x) && is.scalar(x)
}

is.scalar.numeric <- function(x) {
  is.numeric(x) && is.scalar(x)
}

is.scalar.logical <- function(x) {
  is.logical(x) && is.scalar(x)
}

is.scalar <- function(x) {
  identical(length(x), 1L)
}

is.scalar.na <- function(x) {
  all(is.na(x)) & length(x) == 1
}

is.scalar.null <- function(x) {
  all(is.null(x)) & length(x) == 0
}

is.scalar.na.or.null <- function(x) {
  is.scalar.na(x) | is.scalar.null(x)
}

is.scalar.na.or.null.or.length.zero <- function(x) {
  is.scalar.na(x) | is.scalar.null(x) | length(x) == 0
}


is.integerlike <- function(x) {
  all(round(x) == x)
}

is.scalar.integerlike <- function(x) {
  is.scalar(x) && is.integerlike(x)
}

# Mathematical

modus <- function(x){
  # Little helper to calculate modus of simple vector
  t <- table(x)
  as(names(t[t == max(t)]), class(x))

}

#' @export
value_to_vec <- function(value_str,
                         type = c("integer", "character", "double"),
                         collapse = "",
                         simplify = T){
  type <- match.arg(type)
  if(type == "integer"){
    ret <- stringr::str_extract_all(value_str, "[0-9]+") %>% lapply(as.integer)
    if(length(value_str) == 1  && simplify){
      ret <- ret[[1]]
    }
    return(ret)
  }
  else if(type == "double"){
    ret <- stringr::str_extract_all(value_str, "[0-9\\.]+") %>% lapply(as.numeric)
    if(length(value_str) == 1 && simplify){
      ret <- ret[[1]]
    }
    return(ret)
  }
  else{
    return(stringr::str_remove_all(value_str, "[\\[\\]\\{\\}\\(\\)]") %>% str_replace_all("[,]+", collapse))

  }
}
