


# Tversky



tversky_sim <- function(stimuli_pitch, sung_recall_pitch, alpha = 1, beta = 1) {


  stimuli_pitch_intervals <- diff(stimuli_pitch)
  stimuli_pitch_ngrams <- get_ngrams_multiple_sizes2(stimuli_pitch_intervals, M = 8)

  sung_recall_pitch_intervals <- diff(sung_recall_pitch)
  sung_recall_pitch_ngrams <- get_ngrams_multiple_sizes2(sung_recall_pitch_intervals, M = 8)

  intersect_ngrams <- tibble::tibble(value = intersect(stimuli_pitch_ngrams$value, sung_recall_pitch_ngrams$value))

  salience_intersect_ngrams <- get_salience(intersect_ngrams)
  salience_stimuli_ngrams <- get_salience(stimuli_pitch_ngrams)
  salience_sung_recall_ngrams <- get_salience(sung_recall_pitch_ngrams)

  salience_intersect_ngrams / (salience_intersect_ngrams + alpha *  salience_stimuli_ngrams + beta *  salience_sung_recall_ngrams)
}


get_salience <- function(ngrams) {

  idfs <- get_idf_of_ngrams(ngrams$value)
  tfs <- get_tfs_ngrams(ngrams)

  joined_idfs_and_tfs <- idfs %>%
    left_join(tfs, by = "ngram")

  sum( sqrt( joined_idfs_and_tfs$idf * sqrt ( joined_idfs_and_tfs$tf * joined_idfs_and_tfs$tf ) ), na.rm = TRUE) / sum(joined_idfs_and_tfs$idf, na.rm = TRUE)
}



get_tfs_ngrams <- function(ngrams) {

  ngram_count <- ngrams %>%
    count(value) %>%
    arrange(desc(n))

  ngram_count %>%
    rename(ngram = value,
           tf = n)
}





get_idf_of_ngrams <- function(ngrams) {

  purrr::map_dfr(ngrams, function(ngram) {

    no_corpus_melodies_containing_ngram <- Berkowitz::ngram_item_bank %>%
      tibble::as_tibble() %>%
      dplyr::filter(melody %in% ngram) %>%
      dplyr::pull(parent_abs_melody) %>%
      unique() %>%
      length()

    tibble::tibble(ngram = ngram,
                   idf = log(nrow(Berkowitz::phrase_item_bank)/no_corpus_melodies_containing_ngram)) %>%
      mutate(idf = dplyr::case_when(is.infinite(idf) ~ as.numeric(NA), TRUE ~ idf))
  })

}



get_ngrams_multiple_sizes2 <- function (abs_melody, M) {
  if (length(abs_melody) == 1) {
    ngrams.multi <- tibble::tibble(start = NA, N = 1, value = paste(abs_melody, collapse = ","))
  }
  else if (length(abs_melody) == 2) {
    ngrams.multi <- tibble::tibble(start = NA, N = 2, value = paste(abs_melody, collapse = ","))
  }
  else {
    if (length(abs_melody) < M) {
      M <- length(abs_melody)
    }
    ngrams.multi <- purrr::map_dfr(1:M, itembankr::get_all_ngrams, x = abs_melody)
  }
  ngrams.multi
}





# N.B: I added some ngrams that are definitely in Berkowitz::ngram_item_bank, and that there is an intersection
# stimuli_pitch <- c(67, 64, 62, 67, 67, 64, 62, 67, 67, 64, 62, 64, 60, 62, 60, 59, 57)
# sung_recall_pitch <- c(55, 60, 62, 64, 64, 60, 62, 64, 60, 61, 62, 64, 65, 65, 65, 62, 64, 65, 62, 58, 60, 62, 60, 59, 57)


# tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 1, beta = 1)
# 0.3170575

# tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 1, beta = 2)
# 0.239812

# tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 2, beta = 1)
# 0.2329876


# Update:  IDF is defined as:  log(#corpus_melodies / #corpus_melodies_containing_ngram)
