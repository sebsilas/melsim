


#' Compute the melodic similarity of 2 or more melodies
#'
#' @param melody1 The first melody - or set of melodies - for comparison.
#' @param melody2 The second melody - or set of melodies - for comparison.
#' @param input_type The format in which the melodies are being represented initially.
#' @param query_mode The manner in which melody comparisons are made.
#' @param similarity_algorithm Which similarity algorithm to use.
#' @param ngram_length For set-based similarity, the N-gram length.
#' @param transform  For set-based similarity, the transform to use.
#'
#' @return
#' @export
#'
#' @examples
melsim <- function(melody1,
                   melody2,
                   input_type = c("midi_notes_and_durations",
                                  "midi_file",
                                  "mcsv_file",
                                  "musicxml_file",
                                  "audio_file",
                                  "humdrum_file"),
                   query_mode = c("one_to_one", "one_to_many", "many_to_many"),
                   similarity_algorithm = c("opti3",
                                            "edit_distance",
                                            "needleman_wunsch",
                                            "compression_distance_gzip",
                                            "set_based",
                                            "correlation"),
                   ngram_length = 3L,
                   transform = NULL) {

  input_type <- match.arg(input_type)
  similarity_algorithm <- match.arg(similarity_algorithm)


}
