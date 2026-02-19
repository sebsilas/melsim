
library(tidyverse)
library(Berkowitz)
library(itembankr)

load_all(".")

# Helper to build canonical token dataframe from item bank

build_tokens_from_itembank <- function(item_bank, transform) {

  map_dfr(seq_len(nrow(item_bank)), function(i) {

    row <- item_bank[i, ]

    onset_vec    <- str_mel_to_vector(row$onset)
    pitch_vec    <- str_mel_to_vector(row$melody)
    duration_vec <- str_mel_to_vector(row$durations)

    min_len <- min(length(onset_vec),
                   length(pitch_vec),
                   length(duration_vec))

    if(min_len == 0) return(NULL)

    mel <- melody_factory$new(
      mel_data = tibble(
        onset    = onset_vec[seq_len(min_len)],
        pitch    = pitch_vec[seq_len(min_len)],
        duration = duration_vec[seq_len(min_len)]
      ),
      override = TRUE
    )

    mel$add_transforms(sim_transformations, override = TRUE)

    tibble(
      id = i,
      value = mel$data[[transform]]
    )
  }) %>%
    filter(!is.na(value))
}


# N-gram DB builder (aligned with melody_factory)

make_ngram_db_from_melody_factory <- function(item_bank,
                                              transform = "int",
                                              N = 10) {

  token_df <- build_tokens_from_itembank(item_bank, transform) %>%
    mutate(value = as.character(value))

  token_df <- token_df %>%
    group_by(id) %>%
    mutate(pos = row_number()) %>%
    ungroup()

  # Build higher-order n-grams via parkR stack
  bs <- parkR:::build_bigram_stack(
    token_df$value,
    max_level = max(1, N - 1),
    sd_threshold = 0,
    ids = token_df$id
  )

  # Unigram stats
  unigram_stats <- token_df %>%
    group_by(value) %>%
    summarise(
      DF   = n_distinct(id),
      n_xy = n(),
      N    = 1,
      .groups = "drop"
    )

  full_db <- unigram_stats %>%
    bind_rows(bs) %>%
    filter(!is.na(value)) %>%
    mutate(
      idf = log(n_distinct(token_df$id) / DF)
    ) %>%
    distinct(value, DF, n_xy, N, idf)

  attr(full_db, "melsim_version") <- as.character(packageVersion("melsim"))
  attr(full_db, "transform") <- transform
  attr(full_db, "ngram_max") <- N

  return(full_db)
}

# Build all Berkowitz representations

int_ngrams_berkowitz <-
  make_ngram_db_from_melody_factory(
    Berkowitz::phrase_item_bank,
    transform = "int",
    N = 10
  )

ioi_class_ngrams_berkowitz <-
  make_ngram_db_from_melody_factory(
    Berkowitz::phrase_item_bank,
    transform = "ioi_class",
    N = 10
  )

duration_class_ngrams_berkowitz <-
  make_ngram_db_from_melody_factory(
    Berkowitz::phrase_item_bank,
    transform = "duration_class",
    N = 10
  )

int_X_ioi_class_ngrams_berkowitz <-
  make_ngram_db_from_melody_factory(
    Berkowitz::phrase_item_bank,
    transform = "int_X_ioi_class",
    N = 10
  )



usethis::use_data(
  int_ngrams_berkowitz,
  ioi_class_ngrams_berkowitz,
  duration_class_ngrams_berkowitz,
  int_X_ioi_class_ngrams_berkowitz,
  overwrite = TRUE
)

