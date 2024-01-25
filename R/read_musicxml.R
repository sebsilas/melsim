# read_musicxml <- function(musicxml_file) {
#   data <- XML::xmlParse(musicxml_file)
#   xml_data <- XML::xmlToList(data)
#   part <- xml_data$part
#   tempo <- part$measure$direction$sound["tempo"] %>% as.numeric()
#
#   if(length(tempo) == 0) {
#     tempo <- 120
#   }
#   part <- part[names(part) == "measure"]
#   notes <- lapply(part, function(measure) measure[names(measure) == "note"])
#   notes <- unlist(notes, recursive = FALSE)
#
#   ret <- purrr::map_dfr(notes, function(note) {
#
#     # Pitches
#     sci_no <- paste0(note$pitch$step, note$pitch$octave)
#     midi_note <- as.integer(itembankr::sci_notation_to_midi(sci_no))
#
#     # Durations
#     rel_bpm <- as.numeric(note$duration)
#
#         duration <- rel_bpm_to_seconds(rel_bpm, bpm = tempo)
#
#     tibble::tibble(pitch = midi_note,
#                    duration = duration)
#
#   })
#   ret %>% mutate(onset = cumsum(c(0, duration[1:(length(duration)-1)])))
# }
#
#
# rel_bpm_to_seconds <- function(v, bpm) {
#   # Convert note lengths as relative numbers (i.e 4 = semibreve, 2 = minim, 1 = crotchet)
#   # to abs values in seconds, based on a bpm
#   seconds_per_beat <- bpm_to_seconds_per_beat(bpm)
#   seconds_per_beat/v
# }
#
# bpm_to_seconds_per_beat <- function(bpm) {
#   seconds_in_a_minute <- 60
#   seconds_per_beat <- seconds_in_a_minute/bpm
# }
#
#
# # t <- read_musicxml('data-raw/musicxml_test.musicxml')
#
#
#
#
