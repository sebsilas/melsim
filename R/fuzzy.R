

fuzzy <- function(obj) {

  stopifnot(
    is(obj, "interval") || is(obj, "rhythm")
  )

  if(class(obj) == "interval") {
    return(fuzzy_int(obj))
  } else if(class(obj) == "rhythm") {
    return(fuzzy_rhythm(obj))
  } else {
    stop("Class not appropriate.")
  }
}
