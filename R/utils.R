

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

is_class <- function(x, class) {
  if(is.vector(x)){
    x <- as.list(x)
  }
  if(!is.list(x)){
    x <- list(x)
  }
  sapply(x, function(z) methods::is(z, class))
}

safe_get <- function(obj, field) {
  if(field %in% names(obj)) {
    return(obj[[field]])
  }
  return(NULL)
}

remove_cols <- function(data, cols){
  to_remove <- intersect(names(data), cols)
  if(length(to_remove) > 0){
    return(data %>% select(!all_of(to_remove)))
  }
  data
}

#' @export
safe_append <- function(x, y) {
  common_names <- intersect(names(x), names(y))
  if(length(common_names) > 0){
    names(y) <- sapply(names(y), function(n) ifelse(n %in% common_names, sprintf("%s.x", n), n))
  }
  append(x, y)
}

#' @export
expand_grid_unique <- function(x, y, include_equals = FALSE){
  tmp <- tidyr::expand_grid(x = x, y = y)
  if(include_equals) return(tmp %>% filter(x <= y))
  tmp %>% filter(x < y)
}

#' @export
pair_index <- function(s1, s2, sep = "_") {
  stopifnot(length(s1) == length(s2))
  ret <- vector("character", length(s1))
  ret[s1 <= s2] <- sprintf("%s%s%s", s1[s1 <= s2], sep, s2[s1 <= s2])
  ret[s1 > s2] <- sprintf("%s%s%s", s2[s1 > s2], sep, s1[s1 > s2])
  ret
}

#' @export
value_to_vec <- function(value_str,
                         type = c("integer", "character", "numeric", "string"),
                         collapse = "",
                         simplify = T){
  type <- match.arg(type)
  if(type == "integer"){
    ret <- stringr::str_extract_all(value_str, "[+-]?[0-9]+") %>% lapply(as.integer)
    if(length(value_str) == 1  && simplify){
      ret <- ret[[1]]
    }
    return(ret)
  }
  else if(type == "numeric"){
    ret <- stringr::str_extract_all(value_str, "[+-]?[0-9\\.]+") %>% lapply(as.numeric)
    if(length(value_str) == 1 && simplify){
      ret <- ret[[1]]
    }
    return(ret)
  }
  else if(type == "string"){
    return(stringr::str_remove_all(value_str, "[\\[\\]\\{\\}\\(\\)]") %>% str_replace_all("[,]+", collapse))

  }
  else{
    ret <- stringr::str_remove_all(value_str, "[\\[\\]\\{\\}\\(\\)]") %>% str_split(collapse)
    if(length(value_str) == 1 && simplify){
      ret <- ret[[1]]
    }
    return(ret)
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
#safe_entropy <- function(x, ...) suppressWarnings(entropy::entropy(na.omit(x)))

squeeze <- function(x, min_val = 0, max_val = 1){
  pmax(pmin(x, max_val), min_val)
}

modus <- function(x){
  # Little helper to calculate modus of simple vector
  t <- table(x)
  methods::as(names(t[t == max(t)]), class(x))

}

top_n <- function(x, n = 3){
  # Little helper to calculate modus of simple vector
  t <- table(x)
  top_n <- sort(t, decreasing = T)[1:n]
  top_n <- top_n[!is.na(top_n)]
  methods::as(names(top_n), class(x))

}

parse_linear_combination <- function(lin_comb) {

  lin_comb <- as.character(lin_comb)[2]

  elts <- stringr::str_split(lin_comb, "[+-]")[[1]]
  ops <- stringr::str_extract_all(lin_comb, "[+-]")[[1]]
  if(nzchar(elts[[1]])) {
    ops <- c("+", ops)
  } else{
    elts <- elts[2:length(elts)]
  }
  elts <- stringr::str_split_fixed(elts, "[*]", 2)
  if(any(!nzchar(elts)) || any(is.na(suppressWarnings(as.numeric(elts[, 1]))))) {
    to_fix <- which(apply(elts, 1, function(x) nzchar(x[[1]]) & !nzchar(x[[2]])))
    for(i in to_fix) {
      elts[i, 2] <- elts[i, 1]
      elts[i, 1] <- "1"
    }
    if(any(!nzchar(elts)) || any(is.na(suppressWarnings(as.numeric(elts[, 1]))))){
      logging::logerror(sprintf("Invalid linear combination: %s", lin_comb))
      return(NULL)
    }
    #logging::logwarn(sprintf("Fixed terms with no coefficient (%s)", lin_comb))
    #return(NULL)
  }

  tibble(terms =  trimws(elts[, 2]),
         weights  = sprintf("%s%s", ops, trimws(elts[, 1])) %>% as.numeric())
}

validate_sim_measure <- function(sim_measure) {
  if(purrr::is_formula(sim_measure)) {
    return(TRUE)
  }

  if(methods::is(sim_measure, "SimilarityMeasure")){
    return(TRUE)
  }

  if(is.character(sim_measure)){
    return(TRUE)
  }
  return(FALSE)
}

plot_dtw_alignment <- function(x, y = NULL, beta = .5) {
  if(class(x) == "dtw") {
    d <- x
    x <- d$query %>% as.vector()
    y <- d$reference %>% as.vector()
  } else {
    if(is.null(y)) {
      stop("y must have value")
    }
    d <- dtw::dtw(x, y, keep.internals = TRUE)
  }
  plot_df <- bind_rows(tibble(x = x, type = "query"), tibble(x = y, type = "reference"))
  plot_df2 <- tibble(x = x[d$index1], y = y[d$index2]) %>% mutate(d = x - y)

  q <- plot_df %>% ggplot(aes(x = x, y  = type, colour = type)) + geom_point(size = 5)
  q <- q + geom_segment(data = plot_df2,
                        aes(x = x, y = "query", xend = y, yend = "reference"),
                        colour = "black",
                        arrow = arrow(length = unit(0.30, "cm"),
                                      ends = "last",
                                      type = "closed"))
  q <- q + theme_minimal()
  q <- q + labs(x = "Time (s)", title = sprintf("DTW: dist = %.2f, norm = %.2f, d = %.2f, sim(d) = %.2f",
                                                d$distance, d$normalizedDistance,
                                                mean(plot_df2$d), exp(-beta * d$normalizedDistance)))
  q <- q + theme(legend.title = element_blank())
  q

}


rescale_vectors <- function(x, y, rescale_independently = TRUE) {
  # Min-max rescaling function for a single vector
  rescale <- function(vec) {
    (vec - min(vec, na.rm = T)) / (max(vec, na.rm = T) - min(vec, na.rm = T))
  }

  if (rescale_independently) {
    # Rescale x and y independently
    x_rescaled <- rescale(x)
    y_rescaled <- rescale(y)
  } else {
    # Rescale x and y dependently (using the combined min and max)
    combined_min <- min(c(x, y), na.rm = T)
    combined_max <- max(c(x, y), na.rm = T)

    x_rescaled <- (x - combined_min) / (combined_max - combined_min)
    y_rescaled <- (y - combined_min) / (combined_max - combined_min)
  }

  return(list(x_rescaled = x_rescaled, y_rescaled = y_rescaled))
}


one_hot_encode <- function(v) {

  df <- tibble::tibble(v = as.factor(v))

  model.matrix(~v-1, data = df) %>%
    as.data.frame() %>% # Get rid of attributes
    as.matrix()
}


invariance_table <- tribble(
  ~transformation, ~is_transposition_invariant, ~ is_tempo_invariant,
  "pitch", FALSE,   TRUE,
  "pc", TRUE,   TRUE,
  "int", TRUE,   TRUE,
  "ioi", TRUE, FALSE,
  "ioi_class", TRUE, FALSE, # Right? Because it's based on absolute durations..?
  "parsons", TRUE,   TRUE,
  "fuzzy_int", TRUE,   TRUE,
  "implicit_harmonies", FALSE,   TRUE,
  "segmentation", FALSE,   TRUE,
  "int_X_ioi_class", TRUE,   FALSE,  # Right? Because it's based on absolute durations..?
  "duration_class", TRUE, FALSE, # Right? Because it's based on absolute durations..?
  "ngrams", NA, NA, # It depends on what the underlying transform is
  "phrase_segmentation", FALSE, TRUE
)

is_transposition_invariant <- function(transform, optimizer = NULL) {

  if(is.null(optimizer)) {

    ret <- invariance_table %>%
      filter(transformation == !! transform) %>%
      pull(is_transposition_invariant)

    return(ret)
  }
  if(is.na(optimizer) || is.null(optimizer)) {
    return(FALSE)
  }
  else if (optimizer == "transpose") {
    return(TRUE)
  }

}

is_tempo_invariant <- function(transform) {
  invariance_table %>%
    filter(transformation == !! transform) %>%
    pull(is_tempo_invariant)
}

