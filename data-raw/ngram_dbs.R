#library(tidyverse)

make_berkowitz_bigram_stack <- function(N = 8,
                                        item_bank = Berkowitz::phrase_item_bank,
                                        col = "melody"){

  vecs <- stringr::str_split(item_bank[[col]], ",")
  int_data <-
    map_dfr(1: nrow(item_bank), function(i){
      vec <- c(vecs[[i]], NA)
      id <- i
      tibble(int_raw = vec, id = i)
    })
  bs <- parkR:::build_bigram_stack(int_data$int_raw,
                                   max_level = max(1, N - 1),
                                   sd_threshold = 0,
                                   ids = int_data$id)
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
           N = 1,
           bigram_id = sprintf("%s-0", bi_enc)) %>% ungroup() %>%
    bind_rows(bs) %>%
    filter(!stringr::str_detect(value, "NA")) %>%
    mutate(idf = log(nrow(Berkowitz::phrase_item_bank)/DF))
}

int_ngrams_berkowitz <-  make_berkowitz_bigram_stack(N = 10) %>%
  distinct(value, DF, n_xy, f_xy, N, idf)


usethis::use_data(int_ngrams_berkowitz, overwrite = TRUE)


# Fix ioi bug(s)
berk_tmp <- Berkowitz::phrase_item_bank %>%
  as_tibble() %>%
  rowwise() %>%
  mutate(ioi = paste0(diff(itembankr::str_mel_to_vector(onset)), collapse = ","),
         ioi_class = paste0(classify_duration( itembankr::str_mel_to_vector(ioi) ), collapse = ","),
         dur_class = paste0(classify_duration( itembankr::str_mel_to_vector(durations) ), collapse = ",") ) %>%
  ungroup()


ioi_class_ngrams_berkowitz <-  make_berkowitz_bigram_stack(N = 10, berk_tmp, "ioi_class") %>%
  distinct(value, DF, n_xy, f_xy, N, idf)


dur_class_ngrams_berkowitz <-  make_berkowitz_bigram_stack(N = 10, berk_tmp, "dur_class") %>%
  distinct(value, DF, n_xy, f_xy, N, idf)


usethis::use_data(ioi_class_ngrams_berkowitz, dur_class_ngrams_berkowitz, overwrite = TRUE)


