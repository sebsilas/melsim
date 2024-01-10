

#' Compute the melodic similarity two melodies
#'
#' @param melody1 The first melody - or set of melodies - for comparison.
#' @param melody2 The second melody - or set of melodies - for comparison.
#' @param input_type The format in which the melodies are being represented initially.
#' @param similarity_algorithm Which similarity algorithm to use.
#' @param ngram_length For set-based similarity, the N-gram length.
#'
#' @return
#' @export
#'
#' @examples
melsim <- function(melody1,
                   melody2,
                   input_type = c("mcsv_file",
                                  "named_list",
                                  "midi_file",
                                  "musicxml_file",
                                  "monophonic_audio_file",
                                  "humdrum_file"),
                   similarity_algorithm = c("edit_distance",
                                            "set_based",
                                            "opti3",
                                            "needleman_wunsch",
                                            "compression_distance_gzip",
                                            "correlation",
                                            get_proxy_sim_measures()
                                            ),
                   ngram_length = 3L) {

  input_type <- match.arg(input_type)
  similarity_algorithm <- match.arg(similarity_algorithm)

  # Instantiate melodies
  melody1_obj <- melody_factory$new()
  melody2_obj <- melody_factory$new()

  if(input_type == "mcsv_file") {
    melody1_obj$read_mcsv(melody1)
    melody2_obj$read_mcsv(melody2)
  }

  # Get features
  melody1_obj$add_features()
  melody2_obj$add_features()
  # Add transforms
  melody1_obj$add_tranforms()
  melody2_obj$add_tranforms()

  # Apply the similarity algorithm
  melodic_similarity <-
    if(similarity_algorithm %in% get_proxy_sim_measures()) {
      get_proxy_sim(melody1_obj, melody2_obj, similarity_algorithm)
    } else {

      switch(similarity_algorithm,
             edit_distance = melody1_obj$edit_sim(melody2_obj),
             set_based = melody1_obj$ngram_similarity(melody2_obj),
             stop("Similarity algorithm not recognised.")
      )
    }

  # Prepare the result
  tibble::tibble(
    melody1 = basename(melody1), # Only matters for files with extensions, but doesn't harm other inputs
    melody2 = basename(melody2),
    melodic_similarity = melodic_similarity,
    similarity_algorithm = similarity_algorithm
  )

}


# melsim('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv', similarity_algorithm = 'edit_distance')
# melsim('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv', similarity_algorithm = 'set_based')


melsim_multiple_algorithms <- function(melody1,
                                       melody2,
                                       input_type = c("mcsv_file",
                                                      "named_list",
                                                      "midi_file",
                                                      "musicxml_file",
                                                      "monophonic_audio_file",
                                                      "humdrum_file"),
                                       algorithms = c("edit_distance", "set_based", get_proxy_sim_measures())  # Leaving out what not available yet
                                       ) {

  input_type <- match.arg(input_type)

  map_dfr(algorithms, function(alg) melsim(melody1, melody2, input_type, alg))
}

# melsim_multiple_algorithms('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv')


mel_sim_one_to_many <- function(melody1,
                                melody_set2,
                                input_type = c("mcsv_file",
                                               "named_list",
                                               "midi_file",
                                               "musicxml_file",
                                               "monophonic_audio_file",
                                               "humdrum_file"),
                                similarity_algorithm = c("edit_distance",
                                                         "set_based",
                                                         "opti3",
                                                         "needleman_wunsch",
                                                         "compression_distance_gzip",
                                                         "correlation",
                                                         get_proxy_sim_measures())) {

  input_type <- match.arg(input_type)
  similarity_algorithm <- match.arg(similarity_algorithm)

  stopifnot(
    length(melody1) == 1L,
    length(melody_set2) > 1L
  )

  map_dfr(melody_set2, function(melody2) melsim(melody1, melody2, input_type, similarity_algorithm))

}

# mel_sim_one_to_many(melody1 = 'data-raw/nokia_829607.csv',
#                     melody_set2 = list.files('data-raw', full.names = TRUE),
#                     input_type = 'mcsv_file')

mel_sim_many_to_many <- function(melody_set,
                                 input_type = c("mcsv_file",
                                                "named_list",
                                                "midi_file",
                                                "musicxml_file",
                                                "monophonic_audio_file",
                                                "humdrum_file"),
                                 similarity_algorithm = c("edit_distance",
                                                          "set_based",
                                                          "opti3",
                                                          "needleman_wunsch",
                                                          "compression_distance_gzip",
                                                          "correlation",
                                                          "feature_based",
                                                          get_proxy_sim_measures())) {

  input_type <- match.arg(input_type)
  similarity_algorithm <- match.arg(similarity_algorithm)

  stopifnot(
    length(melody_set)> 1L
  )

  ret <-
    expand.grid.unique(melody_set, melody_set) %>%
    tibble::as_tibble() %>%
    rename(melody1 = V1,
           melody2 = V2) %>%
    rowwise() %>%
    mutate(result = list(melsim(melody1, melody2, input_type, similarity_algorithm))) %>%
    ungroup() %>%
    pull(result) %>%
    bind_rows()

}

#
# t <- list.files('data-raw', full.names = TRUE) %>%
#   mel_sim_many_to_many()


melsim_many_to_many_multiple_algorithms <- function(melody_set,
                                                    input_type = c("mcsv_file",
                                                                   "named_list",
                                                                   "midi_file",
                                                                   "musicxml_file",
                                                                   "monophonic_audio_file",
                                                                   "humdrum_file"),
                                       algorithms = c("edit_distance", "set_based", get_proxy_sim_measures() ),
                                       pivot_wider = TRUE) {

  input_type <- match.arg(input_type)


  no_comparisons <- (length(melody_set) * length(melody_set)-1) / 2
  no_computations <- no_comparisons * length(algorithms)

  askYesNo(paste0("There will be ", no_computations, "similarity computations. Are you sure you want to continue?"))
  # 924

  ret <- map_dfr(algorithms, function(alg) mel_sim_many_to_many(melody_set, input_type, alg))

  if(pivot_wider) {
    ret <- pivot_wider(ret, names_from = similarity_algorithm, values_from = melodic_similarity)
  }

  return(ret)
}

# t <- list.files('data-raw', full.names = TRUE) %>%
#   melsim_many_to_many_multiple_algorithms()


read_named_list <- function(named_object) {

  stopifnot(
    "abs_melody" %in% names(named_object) || "pitch" %in% names(named_object),
    "durations" %in% names(named_object) || "onset" %in% names(named_object)
  )

  ret <- tryCatch({
    named_object %>%
      tibble::as_tibble()
  }, error = function(err) {
    logging::logerror(err)
    logging::logerror("Make sure your input is correct")
  })

  if(!"onset" %in% names(ret)) {
    ret <- ret %>%
      mutate(onset = cumsum(durations) - durations[1])
  }

  if("abs_melody" %in% names(ret) && ! "pitch" %in% names(ret)) {
    ret <- ret %>%
      rename(pitch = abs_melody)
  }

  ret <- ret %>%
    select(pitch, onset)

  if(nrow(ret) == 1 && grepl(",", ret$pitch)) {
    ret <- ret %>% musicassessr::expand_string_df_row()
  }

  return(ret)

}

# read_named_list(list(abs_melody = 60:65, durations = rep(1, 6)))
#
# Berkowitz::phrase_item_bank %>%
#   tibble::as_tibble() %>%
#   slice(1) %>%
#   read_named_list()


# Get extra melodic features

# Berkowitz::phrase_item_bank %>%
#   tibble::as_tibble() %>%
#   slice(1) %>%
#   read_named_list() %>%
#   mutate(durations = c(onset[1], diff(onset))) %>%
#   musicassessr::to_string_df() %>%
#   itembankr::get_melody_features(abs_melody_name = 'pitch')



# Berkowitz::phrase_item_bank %>%
#   tibble::as_tibble() %>%
#   slice(1) %>%
#   select(abs_melody, durations, onset) %>%
#   musicassessr::expand_string_df_row() %>%
#   rename(note = abs_melody) %>%
#   itembankr::produce_extra_melodic_features()

get_proxy_sim_measures <- function() {
  proxy_sim_measures <- summary(proxy::pr_DB)$distance
  proxy_sim_measures[which(!proxy_sim_measures)] %>% names()
}

get_proxy_sim <- function(melody1_obj, melody2_obj, method = get_proxy_sim_measures() ) {

  method <- match.arg(method)

  pitch_sim <- proxy::simil(melody1_obj$data$pitch, melody2_obj$data$pitch, method = method)
  duration_sim <- proxy::simil(melody1_obj$data$duration, melody2_obj$data$duration, method = method)

  mean(c(pitch_sim, duration_sim), na.rm = TRUE)

}

# Try a proxy package method

# melsim('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv', similarity_algorithm = 'Jaccard')

# melsim('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv', similarity_algorithm = 'Jaccard')

