


mel_list <- readRDS(system.file("extdata/mel_objects.rds",
                                mustWork = TRUE, package = "melsim"))

names(mel_list) <- 1:length(mel_list)

mel1_list <- mel_list[benchmark_data_small$melody1]
mel2_list <- mel_list[benchmark_data_small$melody2]


saveRDS(mel1_list, file = '../../inst/extdata/mel1_list.rds')
saveRDS(mel2_list, file = '../../inst/extdata/mel2_list.rds')

