

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
usethis::use_data(kinder_full, beatles, parker, miles, overwrite = TRUE)
