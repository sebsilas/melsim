
library(tidyverse)

# Base factory constructor
generate_sim_measures <- function(ngram_lengths, configs) {
  map(ngram_lengths, function(n) {
    map(configs, function(cfg) {
      trans <- cfg$parameters$transformation
      sim <- tolower(cfg$sim_measure)

      # Default fallback name
      default_name <- paste0("ng_", trans, "_", sim, "_", n)

      # Determine final name
      name <- if (is.function(cfg$name_fn)) {
        cfg$name_fn(n)
      } else if (!is.null(cfg$name)) {
        paste0(cfg$name, "_", trans, "_", n)
      } else {
        default_name
      }

      # Determine full name
      full_name <- if (is.function(cfg$full_name_fn)) {
        cfg$full_name_fn(n)
      } else {
        paste0("ngram-", trans, "-", n, "-", sim)
      }

      parameters <- modifyList(cfg$parameters, list(ngram_length = n))

      melsim::sim_measure_factory$new(
        full_name = full_name,
        name = name,
        transformation = "ngrams",
        parameters = parameters,
        sim_measure = cfg$sim_measure
      )
    })
  }) |> flatten()
}


stringdot_base_configs <- list(
  list(
    transformation = "int",
    base_name = "diffsd",
    method = "exponential",
    optimizer = NULL
  ),
  list(
    transformation = "pitch",
    base_name = "rawsd",
    method = "exponential",
    optimizer = "transpose"
  )
)


generate_stringdot_measures <- function(ngram_lengths = 1:9, configs) {
  map(ngram_lengths, function(n) {
    map(configs, function(cfg) {
      # Build parameters list dynamically
      params <- list(
        ngram_length = n,
        method = cfg$method,
        transformation = cfg$transformation
      )
      if (!is.null(cfg$optimizer)) {
        params$optimizer <- cfg$optimizer
      }

      # Unique name and full_name
      name <- paste0(cfg$base_name, n)
      full_name <- paste0(cfg$transformation, "-stringdot-", n)

      # Create the measure
      melsim::sim_measure_factory$new(
        full_name = full_name,
        name = name,
        transformation = cfg$transformation,
        parameters = params,
        sim_measure = "stringdot_utf8"
      )
    })
  }) |> flatten()
}



# Configurations
sim_configs <- list(

  # Tversky variants
  list(
    name = "ngrtvers",
    sim_measure = "Tversky",
    parameters = list(transformation = "int",
                      ngram_db = "melsim::int_ngrams_berkowitz",
                      alpha = 1,
                      beta = 1)
  ),
  list(
    name = "ngrtvera",
    sim_measure = "Tversky",
    parameters = list(transformation = "int",
                      ngram_db = "melsim::int_ngrams_berkowitz",
                      alpha = "auto",
                      beta = "auto")
  ),

  list(
    name = "ngrtvers",
    sim_measure = "Tversky",
    parameters = list(transformation = "duration_class",
                      ngram_db = "melsim::dur_class_ngrams_berkowitz",
                      alpha = 1,
                      beta = 1)
  ),
  list(
    name = "ngrtvera",
    sim_measure = "Tversky",
    parameters = list(transformation = "duration_class",
                      ngram_db = "melsim::dur_class_ngrams_berkowitz",
                      alpha = "auto",
                      beta = "auto")
  ),

  list(
    name = "ngrtvers",
    sim_measure = "Tversky",
    parameters = list(transformation = "ioi_class",
                      ngram_db = "melsim::ioi_class_ngrams_berkowitz",
                      alpha = 1,
                      beta = 1)
  ),
  list(
    name = "ngrtvera",
    sim_measure = "Tversky",
    parameters = list(transformation = "ioi_class",
                      ngram_db = "melsim::ioi_class_ngrams_berkowitz",
                      alpha = "auto",
                      beta = "auto")
  ),

  # Ukkonen variants with dynamic name
  list(
    name_fn = function(n) paste0("ngrukko", n),
    sim_measure = "ukkon",
    parameters = list(transformation = "int")
  ),

  # Sum common
  list(
    name = "ngrsumc",
    sim_measure = "sum_common",
    parameters = list(transformation = "int"),
    full_name_fn = function(n) paste0("ngram-int-", n, "-sum-common")
  ),

  # Count distinct
  list(
    name = "ngrcoord",
    sim_measure = "count_distinct",
    parameters = list(transformation = "int"),
    full_name_fn = function(n) paste0("ngram-int-", n, "-count-distinct")
  ),

  # Jaccard
  list(
    name = "ngrjacc",
    sim_measure = "Jaccard",
    parameters = list(transformation = "int"),
    full_name_fn = function(n) paste0("ngram-int-", n, "-jaccard")
  ),

  # Distribution similarity (int)
  list(
    name = "ngrdist",
    sim_measure = "distr_sim",
    parameters = list(transformation = "int"),
    full_name_fn = function(n) paste0("ngram-int-", n, "-distribution-similarity")
  ),

  # Distribution similarity (int_X_ioi_class)
  list(
    name = "ngram_int_ioi_dist",
    sim_measure = "distr_sim",
    parameters = list(transformation = "int_X_ioi_class"),
    full_name_fn = function(n) paste0("ngram-int|ioi_class-", n, "-distribution-similarity")
  ),

  # Distribution similarity (ioi_class)
  list(
    name_fn = function(n) paste0("ngram_ioi_class_dist", ifelse(n == 3, "", n)),
    sim_measure = "distr_sim",
    parameters = list(transformation = "ioi_class"),
    full_name_fn = function(n) paste0("ngram-ioi_cass-", n, "-distribution-similarity")
  )
)


# Generate all measures (ngram lengths 1 to 9)
stringdot_measures <- generate_stringdot_measures(1:9, stringdot_base_configs)
all_ngram_measures <- generate_sim_measures(1:9, sim_configs)



manual_measures <- list(

  # This defined progmatically above, but for old time's sake
  melsim::sim_measure_factory$new(full_name = "ngram-int-3-ukkon",
                                  name = "ngrukkon",
                                  transformation = "ngrams",
                                  parameters = list(transformation = "int", ngram_length = 3),
                                  sim_measure = "ukkon"),


  melsim::sim_measure_factory$new(full_name = "implicit-harmonies-edit-distance",
                                  name = "harmcore",
                                  transformation = "implicit_harmonies",
                                  parameters = list(),
                                  sim_measure = "edit_sim"),

  melsim::sim_measure_factory$new(full_name = "ioi-class-edit-distance",
                                  name = "rhytfuzz",
                                  transformation = "ioi_class",
                                  parameters = list(),
                                  sim_measure = "edit_sim_utf8"),

  melsim::sim_measure_factory$new(full_name = "int-edit-distance",
                                  name = "diffed",
                                  transformation = "int",
                                  parameters = list(),
                                  sim_measure = "edit_sim_utf8"),


  melsim::sim_measure_factory$new(full_name = "pat_savage_pmi",
                                  name = "pmips",
                                  transformation = "pitch",
                                  parameters = list(optimizer = "transpose"),
                                  sim_measure = "pmi"),

  melsim::sim_measure_factory$new(full_name = "pitch-edit-distance",
                                  name = "rawed",
                                  transformation = "pitch",
                                  parameters = list(optimizer = "transpose"),
                                  sim_measure = "edit_sim_utf8"),


  melsim::sim_measure_factory$new(full_name = "NCD-int_X_ioiclass",
                                  name = "ncdintioi",
                                  transformation = "int_X_ioi_class",
                                  parameters = list(),
                                  sim_measure = "sim_NCD"),

  melsim::sim_measure_factory$new(full_name = "earth-mover-distance",
                                  name = "emd",
                                  transformation = "none",
                                  parameters = list(beta = .5,
                                                    optimizer = "transpose"),
                                  sim_measure = "sim_emd",
                                  transposition_invariant = TRUE,
                                  tempo_invariant = FALSE),

  melsim::sim_measure_factory$new(full_name = "dtw_normed_dist",
                                  name = "dtw",
                                  transformation = "none",
                                  parameters = list(beta = 0),
                                  sim_measure = "sim_dtw",
                                  transposition_invariant = TRUE,
                                  tempo_invariant = FALSE),

  melsim::sim_measure_factory$new(full_name = "const",
                                  name = "const",
                                  transformation = "none",
                                  parameters = list(),
                                  sim_measure = "const",
                                  transposition_invariant = TRUE,
                                  tempo_invariant = TRUE),

  melsim::sim_measure_factory$new(full_name = "Proxy::Tanimoto Pitch",
                                  name = "tanimoto",
                                  transformation = "pitch",
                                  parameters = list(),
                                  sim_measure = "Tanimoto",
                                  transposition_invariant = FALSE,
                                  tempo_invariant = TRUE)
)

# Combine the manually defined measures with the generated ones
similarity_measures <- c(manual_measures, stringdot_measures, all_ngram_measures)

# Assign unique names
names(similarity_measures) <- map_chr(similarity_measures, ~ .x$name)

similarity_measures <- c(
  similarity_measures,
  melsim::sim_measure_factory$new(full_name = "opti3",
                                  name = "opti3",
                                  transformation = "none",
                                  parameters = list(keep_singles = TRUE),
                                  sim_measure = ~ 0.505 *  ngrukkon + 0.417  * rhytfuzz + 0.24  * harmcore - 0.146 * const,
                                  transposition_invariant = TRUE,
                                  tempo_invariant = TRUE)
)

names(similarity_measures) <- purrr::map_chr(similarity_measures, function(x) x$name)

if(any(duplicated(names(similarity_measures)))) {
  logging::logerror("Duplicate similarity measure")
  print(names(similarity_measures)[duplicated(names(similarity_measures))])
}

usethis::use_data(similarity_measures, overwrite = TRUE)

