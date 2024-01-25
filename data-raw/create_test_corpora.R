kinder_full  <- lapply(list.files("data-raw/kinder_full/",
                                  pattern = "csv",
                                  full.names = T), read_melody)

beatles  <- lapply(list.files("data-raw/beatles/", pattern = "csv", full.names = T), read_melody)
parker  <- lapply(list.files("data-raw/parker/", pattern = "csv", full.names = T), read_melody)
miles <- beatles  <- lapply(list.files("data-raw/miles/", pattern = "csv", full.names = T), read_melody)


usethis::use_data(kinder_full, overwrite = TRUE)
usethis::use_data(beatles, overwrite = TRUE)
usethis::use_data(parker, overwrite = TRUE)
usethis::use_data(miles, overwrite = TRUE)
