


#' PMI similarity measure
#'
#' @param query_pitch (integer vector) query pitch sequence
#' @param target_pitch (integer vector) target pitch sequence
#'
#' @return
#' @export
#'
#' @examples
pmi_original <- function(query_pitch, target_pitch, gapOpening = 12, gapExension = 6) {

  # They use a gap opening penalty of 12 and a gap extension penalty of 6 as parameters.

  pmi <- function(q, t) {
    q_l <- length(q)
    t_l <- length(t)
<<<<<<< HEAD
    # aligned <- Biostrings::pairwiseAlignment(intToUtf8(q),
    #                                          intToUtf8(t),
    #                                          type = "global", # i.e., Needleman-Wunsch
    #                                          gapOpening = gapOpening,
    #                                          gapExtension = gapExentsion)
    aligned <- 0
=======
    aligned <- Biostrings::pairwiseAlignment(intToUtf8(q),
                                             intToUtf8(t),
                                             type = "global", # i.e., Needleman-Wunsch
                                             gapOpening = gapOpening,
                                             gapExtension = gapExension)

>>>>>>> 6439289b3aff2e937ffc2abddd9ccd05d9ac4bc7
    q_aligned <- utf8ToInt(as.character(aligned@pattern))
    t_aligned <- utf8ToInt(as.character(aligned@subject))

    sum(q_aligned == t_aligned) / ((q_l + t_l)/2)
  }
  pmi_edit_sim <- function(q, t){
    edit_sim(intToUtf8(q),
             intToUtf8(t))
  }
  # Run for all transpositions and pick the top
  #tictoc::tic()
  query_pitch <- as.integer(query_pitch - stats::median(query_pitch))
  target_pitch <- as.integer(target_pitch - stats::median(target_pitch))
  query_pitch <- query_pitch + 60 + min(c(query_pitch, target_pitch))
  target_pitch <- target_pitch + 60 + min(c(query_pitch, target_pitch))

  #hints <- get_transposition_hints(query_pitch, target_pitch )
  #tictoc::toc()
  hints <- 0:11
  pmi_res <- purrr::map_dfr(hints, function(transposition) {
    #logging::loginfo(sprintf("Trans: %d", transposition))
    query_pitch <- query_pitch + transposition
    tibble::tibble(transposition = transposition,
                   pmi = pmi(query_pitch, target_pitch))

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






