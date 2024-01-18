
library(tidyverse)


# Tversky



tversky_sim <- function(query_pitch, target_pitch, alpha = 1, beta = 1, max_ngram_size = 3) {
  #browser()
  query_int_ngrams <- get_all_ngrams(diff(query_pitch), N = max_ngram_size) %>% count(value)
  target_int_ngrams <- get_all_ngrams(diff(target_pitch), N = max_ngram_size) %>% count(value)

  intersect_ngrams <- intersect(query_int_ngrams$value, target_int_ngrams$value)
  only_query_ngrams <- setdiff(query_int_ngrams$value, intersect_ngrams)
  only_target_ngrams <- setdiff(target_int_ngrams$value, intersect_ngrams)

  salience_intersect_ngrams <- get_salience(intersect_ngrams)
  salience_only_query_ngrams <- get_salience(only_query_ngrams)
  salience_only_target_ngrams <- get_salience(only_target_ngrams)
  if(!is.numeric(alpha)){
    tfs <- query_int_ngrams %>% filter(value %in% intersect_ngrams)
    alpha <- sum(tfs$n)/nrow(query_int_ngrams)
  }
  if(!is.numeric(beta)){
    tfs <- target_int_ngrams %>% filter(value %in% intersect_ngrams)
    beta <- sum(tfs$n)/nrow(target_int_ngrams)
  }
  salience_intersect_ngrams / (salience_intersect_ngrams + alpha *  salience_only_target_ngrams + beta *  salience_only_query_ngrams)
}


get_salience <- function(ngrams) {

  #idfs <- get_idf_of_ngrams(ngrams$value)
  idfs <- int_ngrams_berkowitz %>%
    filter(value %in% ngrams) %>%
    select(ngram = value, idf)
  return(nrow(idfs))
  return(sum(idfs$idf))

  # tfs <- table(ngrams) %>% as.data.frame() %>% set_names(c("ngram", "tf"))
  #
  #  jit <- idfs %>%
  #    dplyr::left_join(tfs, by = "ngram")
  # #tf_idf <-  jit$idf * jit$tf
  # sum(sqrt(jit$idf * jit$tf), na.rm = TRUE) / sum(jit$idf, na.rm = TRUE)
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


get_idf_of_ngrams <- function(ngrams, item_bank = Berkowitz::ngram_item_bank) {

  ngram_lengths <- str_extract_all(ngrams, ",") %>% sapply(function(x) length(x) + 2)
  item_bank <- as_tibble(item_bank) %>% filter(N %in% unique(ngram_lengths))
  if(nrow(item_bank) == 0){
    return(tibble(ngram = ngrams, idf = 0))
  }
  purrr::map2_dfr(ngrams, ngram_lengths, function(ngram, n) {
    #browser()
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

  print(tversky_sim(stimuli_pitch, stimuli_pitch, alpha = 1, beta = 1))

  print(tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 1, beta = 1))
  print(tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 1, beta = 0))
  print(tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 0, beta = 1))
  print(tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = "auto", beta = "auto"))
  for(i in 1:10) {
    query <- sung_recall_pitch[1:(length(sung_recall_pitch)-i)]
    target <- sung_recall_pitch[(i+1):length(sung_recall_pitch)]
    mel_query <- melody_factory$new(tibble(onset = 1:length(query), pitch = query))
    mel_target <- melody_factory$new(tibble(onset = 1:length(target), pitch = target))
    ed_sim <- mel_query$edit_sim(mel_target)
    ukkon_sim <- mel_query$ngram_similarity(mel_target, method = "sum_common")
    tv_sim <- tversky_sim(query,
                          target,
                          alpha = 1,
                          beta = 1,
                          max_ngram_size = 3)
    print(sprintf("%d - TV: %.3f, ED: %.3f, UKKON: %.3f", i, tv_sim, ed_sim, ukkon_sim))
  }

  # 0.3170575

  # tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 1, beta = 2)
  # 0.239812

  # tversky_sim(stimuli_pitch, sung_recall_pitch, alpha = 2, beta = 1)
  # 0.2329876


  # Update:  IDF is defined as:  log(#corpus_melodies / #corpus_melodies_containing_ngram)

}
