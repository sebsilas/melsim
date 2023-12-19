



#' PMI similarity measure
#'
#' @param stimuli_pitch
#' @param sung_recall_pitch
#'
#' @return
#' @export
#'
#' @examples
pmi <- function(stimuli_pitch, sung_recall_pitch) {

  # They use a gap opening penalty of 12 and a gap extension penalty of 6 as parameters.

  pmi <- function(stimuli_pitch, sung_recall_pitch) {

    stimuli_length <- length(stimuli_pitch)
    sung_recall_length <- length(sung_recall_pitch)

    aligned <- Biostrings::pairwiseAlignment(intToUtf8(stimuli_pitch),
                                             intToUtf8(sung_recall_pitch),
                                             type="global", # i.e., Needleman-Wunsch
                                             gapOpening=12, gapExtension=6)

    stimuli_pitch_aligned <- utf8ToInt(as.character(aligned@pattern))
    sung_recall_pitch_aligned <- utf8ToInt(as.character(aligned@subject))


    no_correct_positions <- sum(stimuli_pitch_aligned == sung_recall_pitch_aligned)


    no_correct_positions / ((stimuli_length + sung_recall_length)/2)
  }

  # Run for all transpositions and pick the top

  pmi_res <- purrr::map_dfr(0:11, function(transposition) {

    sung_recall_pitch <- sung_recall_pitch + transposition
    tibble::tibble(transposition = transposition,
                   pmi = pmi(stimuli_pitch, sung_recall_pitch))

  })

  pmi_res %>%
    dplyr::slice_max(pmi) %>%
    dplyr::pull(pmi) %>%
    unique() # If there is a tie, you get more than one pmi value


}


# stimuli_pitch <- c(60, 61, 62, 66)
# sung_recall_pitch <- c(60, 64, 20, 50, 60, 64, 20, 50)
#
#
# pmi(stimuli_pitch, sung_recall_pitch)

# 0.333333

# # http://fma2018.mus.auth.gr/files/papers/FMA2018_paper_4.pdf
#
# # Paper examples
#
# # Figure 1
#
# stimuli_pitch <- c(67, 64, 62, 67, 67, 64, 62, 67, 67, 64, 62, 64, 60, 62)
# sung_recall_pitch <- c(67, 64, 64, 62, 67, 64, 62, 64, 62, 67, 64, 64, 64, 62)
# pmi(stimuli_pitch, sung_recall_pitch)
# # Paper: 0.56
# # 0.50
#
# # Figure 2
#
#
# stimuli_pitch <- c(64, 62, 60, 62, 60, 60, 60)
# sung_recall_pitch <- c(64, 62, 60, 62, 60, 57, 60)
# pmi(stimuli_pitch, sung_recall_pitch)
# # Paper: 0.86
# # 0.86
#
#
# # Figure 3
#
# stimuli_pitch <- c(60, 62, 64, 62, 60, 62, 64, 57, 62, 64, 65, 64, 62, 64, 65, 59)
# sung_recall_pitch <- c(55, 60, 62, 64, 64, 60, 62, 64, 60, 61, 62, 64, 65, 65, 65, 62, 64, 65, 62)
# pmi(stimuli_pitch, sung_recall_pitch)
# # Paper: 0.73
# # 0.69






