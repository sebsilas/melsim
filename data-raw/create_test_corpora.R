

library(tidyverse)

devtools::load_all()


beatles  <- create_corpus_from_csvs("data-raw/beatles/")
parker  <- create_corpus_from_csvs("data-raw/parker/")
miles  <- create_corpus_from_csvs("data-raw/miles/")
kinder_full  <- create_corpus_from_csvs("data-raw/kinder_full/")

midi_biased_trial <- readRDS("data-raw/midi_biased_trial.rds")
midi <- map(unique(midi_biased_trial$target_id), function(tid){
  tmp <- midi_biased_trial %>% filter(target_id == tid) %>% distinct(target_pitches, target_durations)
  duration <- as.numeric(str_split(tmp$target_durations, ",")[[1]])
  pitch <- as.numeric(str_split(tmp$target_pitches, ",")[[1]])
  onset <- cumsum(c(0, duration[-length(duration)]))
  melody_factory$new(mel_data = tibble(onset = onset, pitch = pitch, duration = duration))
})


muel_frieler_exp1 <- create_corpus_from_midi("data-raw/2004_MuelFrielerPaper/Exp1")


# sim_matrix <- melsim(muel_frieler_exp1)$as_tibble()
#
#
# sim_matrix2 <- sim_matrix |>
#   mutate(is_melody1_orig = grepl("_orig", melody1),
#          is_melody2_orig = grepl("_orig", melody2),
#          target_base = substr(melody1, 1, 1),
#          query_base = substr(melody2, 1, 1),
#          query_is_variation = grepl("_", melody2)
#          ) |>
#   filter(is_melody1_orig,
#          !is_melody2_orig,
#          query_is_variation,
#          target_base == query_base
#          )


muel_frieler_exp1_sim <- get_orig_variation_pairs(muel_frieler_exp1)


# Try again with csvs with have their own bar annotations and thus affect harmcore

muel_frieler_exp1  <- create_corpus_from_csvs("data-raw/2004_MuelFrielerPaper/Exp1/csv_sim",
                                              segmentation = "takt")

muel_frieler_exp2  <- create_corpus_from_csvs("data-raw/2004_MuelFrielerPaper/Exp2/Exp2_csvs",
                                              segmentation = "takt")

# Noting that the folder Daniel gave me of csvs didn't contain aorig.csv and corig.csv.
# Experiment 1 csvs have files of those names though, so I copied them over.
# Makes sense but hope correct?


muel_frieler_exp3  <- create_corpus_from_csvs("data-raw/2004_MuelFrielerPaper/Exp3/Exp3_csvs",
                                              segmentation = "takt")



# Experiment 1

exp1_ground_truth <-
  readxl::read_xls("data-raw/2004_MuelFrielerPaper/Exp1/mean_sim_ratings_Exp1.xls") %>%
  mutate(
    target = file1 %>%
      str_remove("^.*\\\\") %>%      # remove Windows path
      tools::file_path_sans_ext(),          # remove .csv

    query = file2 %>%
      str_remove("^.*\\\\") %>%
      tools::file_path_sans_ext()
  ) |>
  rename(mean_similarity = `Aehnlichkeit auf Skala (1-7)`) |>
  dplyr::select(target, query, mean_similarity)


exp1_sim_matrix <- melsim(muel_frieler_exp1)$as_tibble()


muel_frieler_exp1_sim <- exp1_sim_matrix |>
  mutate(melody2 = toupper(melody2)) |>
  left_join(exp1_ground_truth, by = c("melody1" = "target",
                                     "melody2" = "query") ) |>
  rename(target = melody1,
         query = melody2) |>
  dplyr::select(target, query, sim_opti3, sim_ngrukkon, sim_rhytfuzz, sim_harmcore, mean_similarity) |>
  filter(!is.na(mean_similarity)) |>
  rename(similarity = mean_similarity)


cor(muel_frieler_exp1_sim$sim_opti3, muel_frieler_exp1_sim$similarity)

# Experiment 2

exp2_ground_truth <- read_csv("data-raw/2004_MuelFrielerPaper/Exp2/Similarity_Data_Exp2.csv") |>
  mutate( Standard =  tools::file_path_sans_ext(Standard),
          Variante = tools::file_path_sans_ext(Variante) ) |>
  arrange(Standard, Variante)


exp2_sim_matrix <- melsim(muel_frieler_exp2)$as_tibble()


muel_frieler_exp2_sim <- exp2_sim_matrix |>
  mutate(
    melody1 = case_when(melody1 == "KK1" ~ "kk1", TRUE ~ melody1),
    melody2 = case_when(melody2 == "KK1" ~ "kk1", TRUE ~ melody2)
    ) |>
  left_join(exp2_ground_truth, by = c("melody1" = "Standard",
                                      "melody2" = "Variante")) |>
  filter(!is.na(Mean_similarity)) |>
  rename(target = melody1,
         query = melody2) |>
  dplyr::select(target, query, sim_opti3, sim_ngrukkon, sim_rhytfuzz, sim_harmcore, Mean_similarity) |>
  rename(similarity = Mean_similarity)

cor(muel_frieler_exp2_sim$sim_opti3, muel_frieler_exp2_sim$similarity)

# Experiment 3

exp3_ground_truth <- readxl::read_xls("data-raw/2004_MuelFrielerPaper/Exp3/Exp3_Similarities.xls") |>
  mutate(file1 =  tools::file_path_sans_ext(file1),
         file2 = tools::file_path_sans_ext(file2) )


exp3_sim_matrix <- melsim(muel_frieler_exp3)$as_tibble()


muel_frieler_exp3_sim <- exp3_sim_matrix |>
  mutate(
    melody1 = tolower(melody1),
    melody2 = tolower(melody2)
  ) |>
  left_join(exp3_ground_truth, by = c("melody1" = "file1",
                                      "melody2" = "file2")) |>
  filter(!is.na(`similarity (0-10)`)) |>
  rename(target = melody1,
         query = melody2) |>
  dplyr::select(target, query, sim_opti3, sim_ngrukkon, sim_rhytfuzz, sim_harmcore, `similarity (0-10)`) |>
  rename(similarity = `similarity (0-10)`)


cor(muel_frieler_exp3_sim$sim_opti3, muel_frieler_exp3_sim$`similarity`)

# Test benchmarking function on new measure:

# test_bench <- benchmark_sim_measure_on_muel_frieler_2004("rawed")

test_bench |>
  group_by(experiment) |>
  summarise(r = cor(similarity, sim_rawed)) |>
  ungroup()

usethis::use_data(kinder_full,
                  beatles,
                  parker,
                  miles,

                  muel_frieler_exp1,
                  muel_frieler_exp2,
                  muel_frieler_exp3,

                  muel_frieler_exp1_sim,
                  muel_frieler_exp2_sim,
                  muel_frieler_exp3_sim,

                  overwrite = TRUE)
