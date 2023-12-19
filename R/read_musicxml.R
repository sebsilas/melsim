

musicxml_file_to_notes_and_durations <- function(musicxml_file, relativeMelodies = TRUE, relativeDurations = FALSE) {

  # For some reason musicxml files currently don't add up often, with abs_melody and durations


  data <- XML::xmlParse(musicxml_file)
  xml_data <- XML::xmlToList(data)
  part <- xml_data$part
  tempo <- as.numeric(part$measure$direction$sound["tempo"])
  part <- part[names(part) == "measure"]
  notes <- lapply(part, function(measure) measure[names(measure) == "note"])
  notes <- unlist(notes, recursive = FALSE)
  pitches <- unlist(lapply(notes, function(note) paste0(note$pitch$step, note$pitch$octave)))
  pitches <- sci_notation_to_midi(pitches)
  durations <- lapply(notes, function(note) note$duration)
  converted_pitches_and_durs <- convert_pitches_and_durs(pitches, durations, relativeMelodies, relativeDurations, tempo)
  durations <- converted_pitches_and_durs$durations

  tibble::tibble(abs_melody = paste0(pitches, collapse = ","),
                 durations = paste0(durations, collapse = ","),
                 musicxml_file = basename(musicxml_file),
                 N = length(pitches))
}
