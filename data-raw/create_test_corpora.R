source("R/auxiliary.R")
kinder_full  <- lapply(list.files("data-raw/kinder_full/",
                                  pattern = "csv",
                                  full.names = T), read_melody)

usethis::use_data(kinder_full, overwrite = TRUE)
usethis::use_data(beatles, overwrite = TRUE)
usethis::use_data(parker, overwrite = TRUE)
usethis::use_data(miles, overwrite = TRUE)
