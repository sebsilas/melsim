
library(tidyverse)


# Tversky



tversky_sim <- function(query_ngrams,
                        target_ngrams,
                        alpha = 1,
                        beta = 1,
                        ngram_db = int_ngrams_berkowitz) {
  #query_ngrams <- get_all_ngrams(query, N = max_ngram_size) %>% count(value)
  #target_ngrams <- get_all_ngrams(target, N = max_ngram_size) %>% count(value)

  intersect_ngrams <- intersect(query_ngrams, target_ngrams)
  only_query_ngrams <- setdiff(query_ngrams, intersect_ngrams)
  only_target_ngrams <- setdiff(target_ngrams, intersect_ngrams)
  if(is.character(ngram_db)){
    ngram_db <- eval(parse(text = ngram_db), envir = globalenv())
  }
  salience_intersect_ngrams <- get_salience(intersect_ngrams, ngram_db)
  salience_only_query_ngrams <- get_salience(only_query_ngrams, ngram_db)
  salience_only_target_ngrams <- get_salience(only_target_ngrams, ngram_db)
  if(!is.numeric(alpha)){
    alpha <- sum(prop.table(table(query_ngrams))[intersect_ngrams])
  }
  if(!is.numeric(beta)){
    beta <- sum(prop.table(table(target_ngrams))[intersect_ngrams])
  }
  salience_intersect_ngrams / (salience_intersect_ngrams + alpha *  salience_only_target_ngrams + beta *  salience_only_query_ngrams)
}


get_salience <- function(ngrams, ngram_db = int_ngrams_berkowitz) {
  #idfs <- get_idf_of_ngrams(ngrams$value)
  idfs <- ngram_db %>%
    filter(value %in% ngrams) %>%
    select(ngram = value, idf)
  return(nrow(idfs))
  #return(sum(idfs$idf))

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
    target <- sung_recall_pitch[(i + 1):length(sung_recall_pitch)]
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
