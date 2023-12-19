

#' Classify ioi class (see Frieler.. )
#'
#' @param dur_vec best, should be a ioi vector
#' @param ref_duration
#'
#' @return
#' @export
#'
#' @examples
classify_duration <- function(dur_vec, ref_duration = .5){
  rel_dur <- dur_vec/ref_duration
  rhythm_class <- rep(-2, length(rel_dur))
  #rhythm_class[rel_dur <= .45] <- -2
  rhythm_class[rel_dur > 0.45] <- -1
  rhythm_class[rel_dur > 0.9] <- 0
  rhythm_class[rel_dur > 1.8] <- 1
  rhythm_class[rel_dur > 3.3] <- 2
  rhythm_class
}
