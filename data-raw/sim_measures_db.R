ngram_ukkon <- melsim::sim_measure_factory$new(full_name = "ngram-int-3-ukkon",
                                       name = "ngrukkon",
                                       type = "set_based",
                                       transformation = "ngrams",
                                       parameters = list(transformation = "int", ngram_length = 3),
                                       sim_measure = "ukkon",
                                       transposition_invariant = T,
                                       tempo_invariant = T)

ngram_jaccard <- melsim::sim_measure_factory$new(full_name = "ngram-int-3-jaccard",
                                         name = "ngrjacc",
                                         type = "set_based",
                                         transformation = "ngrams",
                                         parameters = list(transformation = "int", ngram_length = 3),
                                         sim_measure = "Jaccard",
                                         transposition_invariant = T,
                                         tempo_invariant = T)

harmcore <- melsim::sim_measure_factory$new(full_name = "implicit-harmonies-edit-distance",
                                    name = "harmcore",
                                    type = "sequence_based",
                                    transformation = "implicit_harmonies",
                                    parameters = list(),
                                    sim_measure = "edit_distance",
                                    transposition_invariant = T,
                                    tempo_invariant = T)

rhytfuz <- melsim::sim_measure_factory$new(full_name = "ioi-class-edit-distance",
                                   name ="rhytfuz",
                                   type = "sequence_based",
                                   transformation = "ioi_class",
                                   parameters = list(),
                                   sim_measure = "edit_distance",
                                   transposition_invariant = T,
                                   tempo_invariant = T)

similarity_measures <- list("ngram_ukkon" = ngram_ukkon,
                            "ngram_jaccard" = ngram_jaccard,
                            "harmcore" = harmcore,
                            "rhythfuz" = rhytfuz)
usethis::use_data(similarity_measures, overwrite = T)
