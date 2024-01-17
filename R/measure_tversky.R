
library(tidyverse)


# Tversky



tversky_sim <- function(stimuli_pitch, sung_recall_pitch, alpha = 1, beta = 1) {
  browser()
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
  browser()
  idfs <- get_idf_of_ngrams(ngrams$value)
  tfs <- get_tfs_ngrams(ngrams)

  jit <- idfs %>%
    dplyr::left_join(tfs, by = "ngram")
  tf_idf <-  jit$idf * jit$tf
  sum(sqrt(jit$idf * jit$tf), na.rm = TRUE) / sum(jit$idf, na.rm = TRUE)
}



get_tfs_ngrams <- function(ngrams) {

  ngrams %>%
    count(value) %>%
    arrange(desc(n)) %>%
    rename(ngram = value,
           tf = n)
}



get_idf <- function(ngram, ngram_db = Berkowitz::ngram_item_bank){
  if(is.null(ngram_db) || nrow(ngram_db) == 0) return(0)
  ngram_db %>%
    dplyr::filter(melody %in% ngram) %>%
    dplyr::pull(parent_abs_melody) %>%
    unique() %>%
    length()

}

make_berkowitz_bigram_stack <- function(N = 8){
  vecs <- str_split(Berkowitz::phrase_item_bank$melody, ",")
  int_data <-
    map_dfr(1: nrow(Berkowitz::phrase_item_bank), function(i){
    vec <- vecs[[i]]
    id <- i
    tibble(int_raw = vec, id = i)
  })
  bs <- parkR:::build_bigram_stack(int_data$int_raw, max_level = max(1, N - 1), sd_threshold = 0, ids = int_data$id)
  int_data %>%
    rename(value = int_raw) %>%
    group_by(id) %>%
    mutate(pos = 1:n(),
           bi_enc = as.integer(factor(as.character(value))),
           n = nrow(.)) %>%
    ungroup() %>%
    group_by(value) %>%
    mutate(DF = n_distinct(id),
           n_xy = n(),
           f_xy = n_xy/n,
           level = 0,
           bigram_id = sprintf("%s-0", bi_enc)) %>% ungroup() %>%
    bind_rows(bs) %>% mutate(idf = log(nrow(Berkowitz::phrase_item_bank)/DF))
}

get_idf_of_ngrams <- function(ngrams, item_bank = Berkowitz::ngram_item_bank) {

  ngram_lengths <- str_extract_all(ngrams, ",") %>% sapply(function(x) length(x) + 2)
  item_bank <- as_tibble(item_bank) %>% filter(N %in% unique(ngram_lengths))
  if(nrow(item_bank) == 0){
    return(tibble(ngram = ngrams, idf = 0))
  }
  purrr::map2_dfr(ngrams, ngram_lengths, function(ngram, n) {
    browser()
    DF <- get_idf(ngram, item_bank %>% filter(N == n))
    tibble::tibble(ngram = ngram,
                   idf = log(nrow(Berkowitz::phrase_item_bank)/DF)) %>%
      dplyr::mutate(idf = dplyr::case_when(is.infinite(idf) ~ as.numeric(NA), TRUE ~ idf))
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
test_tverski_sim <- function(){
  stimuli_pitch <- c(67, 64, 62, 67, 67, 64, 62, 67, 67, 64, 62, 64, 60, 62, 60, 59, 57)
  sung_recall_pitch <- c(55, 60, 62, 64, 64, 60, 62, 64, 60, 61, 62, 64, 65, 65, 65, 62, 64, 65, 62, 58, 60, 62, 60, 59, 57)


  tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 1, beta = 1)
  # 0.3170575

  # tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 1, beta = 2)
  # 0.239812

  # tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 2, beta = 1)
  # 0.2329876


  # Update:  IDF is defined as:  log(#corpus_melodies / #corpus_melodies_containing_ngram)

}
