#' read_musicxml
#' Simple reader for MusicXML files, works only in the simplest cases and only with most basic information
#'
#' @param musicxml_file (string)
#' @return tibbel with pitch, onset, duration
#' @export
read_musicxml <- function(musicxml_file) {
  data <- XML::xmlParse(musicxml_file)
  xml_data <- XML::xmlToList(data)
  parts <- xml_data[names(xml_data) == "part"]
  if(length(parts) > 1){
    logging::logwarn("MusicXML contains several parts, taking first one")
  }
  parts <- parts[[1]]
  #browser()
  measures <- parts[names(parts) == "measure"]
  tempo <- measures[[1]]$direction$sound["tempo"] %>% as.numeric()
  if(length(tempo) == 0 || is.na(tempo)) {
    tempo <- measures[[1]]$sound$.attrs[["tempo"]] %>% as.numeric()
    if(length(tempo) == 0 || is.na(tempo)){
      logging::logwarn("Could not find tempo information, using 120 bpm")
      tempo <- 120
    }
  }

  beat_duration <- 60./tempo
  division <- measures[[1]]$attributes$divisions %>% as.integer()
  if(length(division) > 1){
    logging::logwarn("MusicXML contains more than one division, using first one")
    division <- division[[1]]

  }
  notes <- lapply(measures, function(m) m[names(m) == "note"])
  notes <- unlist(notes, recursive = FALSE)
  note_code <- c(9, 11, 0, 2, 4, 5, 7)
  names(note_code) <- stringr::str_split(LETTERS[1:7], "")
  ret <- purrr::map_dfr(notes, function(note) {
    # Pitches
    pc <- note_code[note$pitch$step]
    if("alter" %in% names(note$pitch)){
      pc <- pc + as.integer(note$pitch$alter)
    }
    pitch <- pc + (as.integer(note$pitch$octave) + 1) * 12

    # Durations
    duration_in_beats <- as.numeric(note$duration)/division
    duration_in_secs <- duration_in_beats * beat_duration

    tibble::tibble(pitch = pitch,
                   duration = duration_in_secs)

  })
  ret %>% mutate(onset = cumsum(c(0, duration[1:(length(duration)-1)])))
}


