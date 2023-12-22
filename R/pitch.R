

#' Create a pitch class (from a MIDI pitch)
#'
#' @param midi_pitch
#'
#' @return
#' @export
#'
#' @examples
pc <- function(midi_pitch) {

  stopifnot(
    is.scalar.character(midi_pitch) || is.integerlike(midi_pitch)
  )

  if(is.scalar.character(midi_pitch)) {
    midi_pitch <- itembankr::str_mel_to_vector(midi_pitch)
  }

  out <- itembankr::midi_to_pitch_class(midi_pitch)

  attr(out, "class") <- "pitch_class"

  return(out)

}


#' Transpose a MIDI pitch, or set of MIDI pitches.
#'
#' @param midi_pitch The MIDI pitch or pitches to transpose.
#' @param transpose The amount to transpose by as an integer (positive or negative).
#'
#' @return
#' @export
#'
#' @examples
trans <- function(midi_pitch, transpose) {

  stopifnot(
    is.scalar.character(midi_pitch) || is.integerlike(midi_pitch),
    is.integerlike(transpose)
  )

  if(is.scalar.character(midi_pitch)) {
    midi_pitch <- itembankr::str_mel_to_vector(midi_pitch)
  }

  out <- midi_pitch + transpose

  attr(out, "class") <- "pitch"

  return(out)

}


#' Creates
#'
#' @param midi_pitches
#'
#' @return
#' @export
#'
#' @examples
int <- function(midi_pitches) {

  stopifnot(
    is.scalar.character(midi_pitches) || is.integerlike(midi_pitches)
  )

  if(is.scalar.character(midi_pitches)) {
    midi_pitches <- itembankr::str_mel_to_vector(midi_pitches)
  }

  stopifnot(
    length(midi_pitches) > 1L
  )

  out <- diff(midi_pitches)
  # Set interval class
  attr(out, "class") <- "interval"
  out
}



fuzzy_int <- function(intervals) {

  stopifnot(is(intervals, "interval"))

  fuzzy_ints <- purrr::map_int(intervals, function(.x) {

    if(.x < -7) {
      return(-4L)
    } else if(dplyr::between(.x, -7, -5)) {
      return(-3L)
    } else if(dplyr::between(.x, -4, -3)) {
      return(-2L)
    } else if(dplyr::between(.x, -2, -1)) {
      return(-2L)
    } else if(.x == 0) {
      return(0)
    } else if(dplyr::between(.x, 1, 2)) {
      return(1L)
    } else if(dplyr::between(.x, 3, 4)) {
      return(2L)
    } else if(dplyr::between(.x, 5, 7)) {
      return(3L)
    } else if(.x > 7) {
      return(4L)
    } else {
      stop("Something strange happened.")
    }

  })

  attr(fuzzy_ints, "class") <- "fuzzy_interval"

  return(fuzzy_ints)

}


parsons <- function(intervals) {

  stopifnot(is(intervals, "interval"))

  parsons_ints <- purrr::map_int(intervals, function(.x) {

    if(.x < 0) {
      return(-1L)
    } else if(.x == 0) {
      return(0L)
    } else if(.x > 0) {
      return(1L)
    } else {
      stop("Something strange happened.")
    }


  })

  attr(parsons_ints, "class") <- "parsons"

  return(parsons_ints)

}




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





