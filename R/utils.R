

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

is_class <- function(x, class){
  if(is.vector(x)){
    x <- as.list(x)
  }
  if(!is.list(x)){
    x <- list(x)
  }
  sapply(x, function(z) is(z, class))
}

safe_get <- function(obj, field){
  if(field %in% names(obj)) {
    return(obj[[field]])
  }
  return(NULL)
}

#' @export
expand_grid_unique <- function(x, y, include_equals = FALSE){
  tmp <- expand_grid(x = x, y = y)
  if(include_equals) return(tmp %>% filter(x <= y))
  tmp %>% filter(x < y)
}

pair_index <- function(x, y){
  stopifnot(length(x) == length(y))
  if(!is.integer(x)){
    x <- as.integer(factor(x))
  }
  if(!is.integer(y)){
    y <- as.integer(factor(y))
  }
  ret  <- sprintf("%s_%s", x, y)
  ret[x > y] <- ret[x < y]
  ret
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


remove_attributes <- function(x) {
  attributes(x) <- NULL
  return(x)
}

ticks_to_ms <- function(ticks, ppq, tempo) {
  us_per_quarter <- tempo # Tempo in latest Set Tempo event>
  us_per_tick <- us_per_quarter / ppq
  seconds_per_tick <- us_per_tick / 1000000
  seconds <- ticks * seconds_per_tick
  seconds
}

get_division_from_midi_file <- function(file){
  # borrowed from tuneR's readMidi
  con <- file(description = file, open = "rb")
  on.exit(close(con))
  MThd <- readChar(con, 4)
  if(MThd != "MThd")
    stop("No Header Chunk in this Midi (?)")
  MThd_length <- readBin(con, integer(0), n = 1, size = 4, endian = "big")
  if(MThd_length != 6)
    stop("Unexpected Header Chunk size")
  MThd_format <- readBin(con, integer(0), n = 1, size = 2, endian = "big", signed = FALSE)
  if(!(MThd_format %in% 0:2))
    stop("Unexpected Mide file format")
  MThd_tracks <- readBin(con, integer(0), n = 1, size = 2, endian = "big", signed = FALSE)

  # FIXME: MThd_division < 0 can appear in Midi files with a very different interpretation!
  MThd_division <- readBin(con, integer(0), n = 1, size = 2, signed = TRUE, endian = "big")
  if(MThd_division < 0){
    stop("Midi representation of timing: Frames per second / ticks per frame not yet implemented, please ask the author")
  }
  MThd_division
}

# Mathematical

abs_mean <- function(x, ...) mean(abs(x), ...)
abs_sd <- function(x, ...) sd(abs(x), ...)
safe_entropy <- function(x, ...) suppressWarnings(entropy::entropy(na.omit(x)))

modus <- function(x){
  # Little helper to calculate modus of simple vector
  t <- table(x)
  as(names(t[t == max(t)]), class(x))

}

top_n <- function(x, n = 3){
  # Little helper to calculate modus of simple vector
  t <- table(x)
  top_n <- sort(t, decreasing = T)[1:n]
  top_n <- top_n[!is.na(top_n)]
  as(names(top_n), class(x))

}

parse_linear_combination <- function(lin_comb){
  elts <- stringr::str_split(lin_comb, "[+-]")[[1]]
  ops <- stringr::str_extract_all(lin_comb, "[+-]")[[1]]
  if(nzchar(elts[[1]])){
    ops <- c("+", ops)
  }
  else{
    elts <- elts[2:length(elts)]
  }
  elts <- stringr::str_split_fixed(elts, "[*]", 2)

  if(any(!nzchar(elts)) || any(is.na(suppressWarnings(as.numeric(elts[, 1]))))){
    logging::logerror(sprintf("Invalid linear combination: %s", lin_comb))
    return(NULL)
  }

  tibble(terms =  trimws(elts[, 2]),
         weights  = sprintf("%s%s", ops, trimws(elts[, 1])) %>% as.numeric())
}

validate_sim_measure <- function(sim_measure){
  if(is(sim_measure, "SimilarityMeasure")){
    return(TRUE)
  }
  if(!is.character(sim_measure)){
    return(FALSE)
  }
  !is.null(parse_linear_combination(sim_measure))
}
