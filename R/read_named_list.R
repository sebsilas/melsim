
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
