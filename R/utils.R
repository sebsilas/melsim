

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
