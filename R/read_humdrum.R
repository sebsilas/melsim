

read_humdrum <- function(fn) {

  h <- humdrumR::readHumdrum(fn)

  pitches <- h %>% humdrumR::pitch()

  rhythms <- h |> humdrumR::select(Token) |> humdrumR::dur()


  # pitches$Pitch %>%
  #   remove_attributes() %>%
  #   itembankr::sci_notation_to_midi() %>%
  #   tibble::tibble(pitch = .)

  rhythms
}


# setwd(humdrumR::humdrumRroot)
# dir('HumdrumData')
# dir('HumdrumData/BachChorales')

# t <- read_humdrum('HumdrumData/BachChorales/chor001.krn')
