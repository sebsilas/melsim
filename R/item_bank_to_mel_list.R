
#' Convert an itembankr itembank to a list of melsim objects or mutate as new column
#'
#' @param item_bank
#' @param add_as_column_to_item_bank
#'
#' @returns
#' @export
#'
#' @examples
item_bank_to_mel_list <- function(item_bank, add_as_column_to_item_bank = FALSE) {

  item_bank <- item_bank %>% tibble::as_tibble()

  ret <- item_bank %>%
        purrr::pmap(function(...) {
          row <- list(...)
          read_named_list(row) %>%
            melody_factory$new(mel_meta = tibble(name = row$item_id) )
        })

  if(add_as_column_to_item_bank) {
    ret <- item_bank %>%
      mutate(melsim_obj = ret)
  }

  return(ret)

}


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


  if("abs_melody" %in% names(ret) && ! "pitch" %in% names(ret)) {
    ret <- ret %>%
      rename(pitch = abs_melody)
  }

  if("durations" %in% names(named_object)) {
    ret <- ret %>%
      rename(duration = durations)
  }

  if(nrow(ret) == 1 && grepl(",", ret$pitch)) {
    ret <- ret %>% musicassessr::expand_string_df_row()
  } else if(is.scalar.character(ret$pitch) && is_midi_note(ret$pitch)) { # For case of a single note...
    ret <- ret %>%
      dplyr::mutate(dplyr::across(dplyr::everything(), as.numeric))
  }

  if(!"onset" %in% names(ret)) {
    ret <- ret %>%
      mutate(onset = cumsum(duration) - duration[1])
  }

  ret <- ret %>%
    select(pitch, onset, duration)

  return(ret)

}


# t <- read_named_list(list(abs_melody = 60:65, durations = rep(1, 6)))
#
#
# t3 <- Berkowitz::phrase_item_bank %>%
#   as_tibble() %>%
#   slice_head(n = 2) %>%
#   item_bank_to_mel_list(add_as_column_to_item_bank = F)
#
#
# tt <- structure(list(abs_melody = c("55,57,55,64,59,59,55,52", "62,62,55,62,60,60,60,53,60"
# ), durations = c("0.13,0.13,0.26,0.26,0.26,0.26,0.26,0.52", "0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5"
# )), row.names = c(NA, -2L), class = c("tbl_df", "tbl", "data.frame"
# ))
#
#
# tt2 <- tt %>% item_bank_to_mel_list(add_as_column_to_item_bank = F)
#
#
#
# # Breaks:
#
# t2 <- Berkowitz::phrase_item_bank %>%
#   item_bank_to_mel_list()


