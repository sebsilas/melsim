

load_all()


beatles  <- create_corpus_from_csvs("data-raw/beatles/")
# parker  <- create_corpus_from_csvs("data-raw/parker/")
# miles  <- create_corpus_from_csvs("data-raw/miles/")
# kinder_full  <- create_corpus_from_csvs("data-raw/kinder_full/")


usethis::use_data(kinder_full, beatles, parker, miles, overwrite = TRUE)
