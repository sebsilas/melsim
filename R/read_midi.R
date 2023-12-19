

#' Get notes and durations from a MIDI file
#'
#' @param midi_file
#' @param string_df
#' @param produce_extra_melodic_features
#'
#' @return
#' @export
#'
#' @examples
midi_file_to_notes_and_durations <- function(midi_file, string_df = TRUE, produce_extra_melodic_features = TRUE) {

  midi_file_dat <- tuneR::readMidi(midi_file)

  tempo <- midi_file_dat %>%
    dplyr::filter(event == "Set Tempo") %>%
    dplyr::pull(parameterMetaSystem) %>% as.numeric()

  if(length(tempo) != 1) {
    tempo_bpm <- microseconds_per_beat_to_bpm(tempo)
    tempo <- tempo[1]
  }

  notes <- tuneR::getMidiNotes(midi_file_dat) %>%
    tibble::as_tibble()

  out <- notes %>% dplyr::mutate(onset = round(ticks_to_ms(time, ppq = get_division_from_midi_file(midi_file), tempo = tempo), 2),
                                 durations = round(ticks_to_ms(length, ppq = get_division_from_midi_file(midi_file), tempo = tempo), 2)) %>%
    dplyr::select(onset, durations, note) %>%
    dplyr::mutate(midi_file = basename(midi_file),
                  N = nrow(notes))

  if(produce_extra_melodic_features) {
    out <- out %>% produce_extra_melodic_features()
  }
  if(string_df) {
    out <- out %>% musicassessr::to_string_df(exclude_cols = "midi_file")
  }
  out
}
