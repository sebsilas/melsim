
similarity_measures <- list(
  melsim::sim_measure_factory$new(full_name = "ngram-int-3-ukkon",
                                  name = "ngrukkon",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int", ngram_length = 3),
                                  sim_measure = "ukkon"#,
                                  #transposition_invariant = TRUE,
                                  #tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-int-2-ukkon",
                                  name = "ngrukko2",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int", ngram_length = 2),
                                  sim_measure = "ukkon"#,
                                  #transposition_invariant = TRUE,
                                  #tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-int-1-ukkon",
                                  name = "ngrukko1",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int", ngram_length = 1),
                                  sim_measure = "ukkon"#,
                                  #transposition_invariant = TRUE,
                                  #tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-int-3-sum-common",
                                  name = "ngrsumc",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int", ngram_length = 3),
                                  sim_measure = "sum_common"#,
                                  #transposition_invariant = TRUE,
                                  #tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-int-3-count-distinct",
                                  name = "ngrcoord",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int", ngram_length = 3),
                                  sim_measure = "count_distinct"#,
                                  #transposition_invariant = TRUE,
                                  #tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-int-3-jaccard",
                                  name = "ngrjacc",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int", ngram_length = 3),
                                  sim_measure = "Jaccard"#,
                                  #transposition_invariant = TRUE,
                                  #tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-int-3-distribution-similarity",
                                  name = "ngrdist",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int", ngram_length = 3),
                                  sim_measure = "dist_sim"#,
                                  #transposition_invariant = TRUE,
                                  #tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-int|ioi_cass-3-distribution-similarity",
                                  name = "ngram_int_ioi_dist",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int_X_ioi_class", ngram_length = 3),
                                  sim_measure = "dist_sim"#,
                                  #transposition_invariant = TRUE,
                                  #tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-ioi_cass-3-distribution-similarity",
                                  name = "ngram_ioi_class_dist",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "ioi_class", ngram_length = 3),
                                  sim_measure = "dist_sim"#,
                                  #transposition_invariant = TRUE,
                                  #tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-ioi_cass-2-distribution-similarity",
                                  name = "ngram_ioi_class_dist2",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "ioi_class", ngram_length = 2),
                                  sim_measure = "dist_sim"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-ioi_cass-1-distribution-similarity",
                                  name = "ngram_ioi_class_dist1",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "ioi_class", ngram_length = 1),
                                  sim_measure = "dist_sim"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "implicit-harmonies-edit-distance",
                                  name = "harmcore",
                                  #type = "sequence_based",
                                  transformation = "implicit_harmonies",
                                  parameters = list(),
                                  sim_measure = "edit_sim"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ioi-class-edit-distance",
                                  name ="rhytfuzz",
                                  #type = "sequence_based",
                                  transformation = "ioi_class",
                                  parameters = list(),
                                  sim_measure = "edit_sim_utf8"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "int-edit-distance",
                                  name ="diffed",
                                  #type = "sequence_based",
                                  transformation = "int",
                                  parameters = list(),
                                  sim_measure = "edit_sim_utf8"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-int-3-tversky",
                                  name = "ngrtvers",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int",
                                                    ngram_length = 3,
                                                    ngram_db = "melsim::int_ngrams_berkowitz",
                                                    alpha = 1,
                                                    beta = 1),
                                  sim_measure = "Tversky"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "ngram-int-3-tversky",
                                  name = "ngrtvera",
                                  #type = "set_based",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int",
                                                    ngram_length = 3,
                                                    ngram_db = "melsim::int_ngrams_berkowitz",
                                                    alpha = "auto",
                                                    beta = "auto"),
                                  sim_measure = "Tversky"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "pat_savage_pmi",
                                  name = "pmips",
                                  #type = "sequence_based",
                                  transformation = "pitch",
                                  parameters = list(optimizer = "transpose"),
                                  sim_measure = "pmi"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "pitch-edit-distance",
                                  name = "rawed",
                                  #type = "sequence_based",
                                  transformation = "pitch",
                                  parameters = list(optimizer = "transpose"),
                                  sim_measure = "edit_sim_utf8"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = TRUE
                                  ),

  melsim::sim_measure_factory$new(full_name = "NCD-int_X_ioiclass",
                                  name = "ncdintioi",
                                  #type = "special",
                                  transformation = "int_X_ioi_class",
                                  parameters = list(),
                                  sim_measure = "sim_NCD"#,
                                  # transposition_invariant = TRUE,
                                  # tempo_invariant = FALSE
                                  ),

  melsim::sim_measure_factory$new(full_name = "earth-mover-distance",
                                  name = "emd",
                                  #type = "special",
                                  transformation = "none",
                                  parameters = list(beta = .5,
                                                    optimizer = "transpose"),
                                  sim_measure = "sim_emd",
                                  transposition_invariant = TRUE,
                                  tempo_invariant = FALSE
                                  ),

  melsim::sim_measure_factory$new(full_name = "dtw_normed_dist",
                                  name = "dtw",
                                  #type = "special",
                                  transformation = "none",
                                  parameters = list(beta = 0),
                                  sim_measure = "sim_dtw",
                                  transposition_invariant = TRUE,
                                  tempo_invariant = FALSE
                                  ),

  melsim::sim_measure_factory$new(full_name = "const",
                                  name = "const",
                                  #type = "special",
                                  transformation = "none",
                                  parameters = list(),
                                  sim_measure = "const",
                                  transposition_invariant = TRUE,
                                  tempo_invariant = TRUE
                                  ),
  melsim::sim_measure_factory$new(full_name = "Proxy::Tanimoto Pitch",
                                  name = "tanimoto",
                                  #type = "special",
                                  transformation = "pitch",
                                  parameters = list(),
                                  sim_measure = "Tanimoto",
                                  transposition_invariant = FALSE,
                                  tempo_invariant = TRUE
  )

  )

# Add names
names(similarity_measures) <- purrr::map_chr(similarity_measures, function(x) x$name)

# Add linear combinations after (because they require that the constituent measures are already defined.)


similarity_measures <- c(
  similarity_measures,
  melsim::sim_measure_factory$new(full_name = "opti3",
                                  name = "opti3",
                                  #type = "linear_combination",
                                  transformation = "none",
                                  parameters = list(keep_singles = TRUE),
                                  sim_measure = ~ 0.505 *  ngrukkon + 0.417  * rhytfuzz + 0.24  * harmcore - 0.146 * const,
                                  transposition_invariant = TRUE,
                                  tempo_invariant = TRUE)
)

# Do names again
names(similarity_measures) <- purrr::map_chr(similarity_measures, function(x) x$name)

if(length(unique(names(similarity_measures))) != length(names(similarity_measures))){
  logging::logerror("Duplicate similarity measure")
}


usethis::use_data(similarity_measures, overwrite = TRUE)

