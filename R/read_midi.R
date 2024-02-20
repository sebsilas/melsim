


read_midi <- function(midi_file) {
  #stop("Not implemeented")
  midi_file_dat <- tuneR::readMidi(midi_file)

  tempo <- midi_file_dat %>%
    dplyr::filter(event == "Set Tempo") %>%
    dplyr::pull(parameterMetaSystem) %>%
    as.numeric()

  if(length(tempo) != 1) {
    tempo_bpm <- 60000000/tempo
    tempo <- tempo[1]
  }

  notes <- tuneR::getMidiNotes(midi_file_dat) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(onset = round(ticks_to_ms(time, ppq = get_division_from_midi_file(midi_file), tempo = tempo), 2),
                  durations = round(ticks_to_ms(length, ppq = get_division_from_midi_file(midi_file), tempo = tempo), 2)) %>%
    dplyr::select(onset, durations, note) %>%
    dplyr::rename(pitch = note)
  notes
}

# Utils



#t <- read_midi('data-raw/Berkowitz623.mid')
