ukkon_similarity <- function (x, y, na.rm = T){
  if(na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  }
  joint <- c(x, y) %>% table()
  tx <- factor(x, levels = names(joint)) %>% table()
  ty <- factor(y, levels = names(joint)) %>% table()
  1 - sum(abs(tx - ty))/(length(x) + length(y))
}

count_distinct <- function(x, y, na.rm = T){
  if(na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  }
  length(intersect(x, y))/max(length(x), length(y))
}

sum_common <- function(x, y, na.rm = T){
  if(na.rm){
    x <- na.omit(x)
    y <- na.omit(y)
  }
  joint <- c(x, y) %>% table()
  tx <- factor(x, levels = names(joint)) %>% table()
  ty <- factor(y, levels = names(joint)) %>% table()
  is <- intersect(names(tx[tx > 0]), names(ty[ty > 0]))
  sum(tx[is] + ty[is])/(length(x) + length(y))
}

#' An ngrukkon wrapper to produce warnings and return NAs rather than stop if one entry is too short
#'
#' @param x
#' @param y
#' @param N
#'
#' @return
#' @export
#'
#' @examples
ngrukkon_safe <- function(x, y, N = 3) {
  ngrukkon_warning(x)
  ngrukkon_warning(y)
  if( length(x) < N | length(y) < N ) {
    res <- NA
  } else {
    res <- ngrukkon(x, y, N)
  }
  res
}

ngrukkon_warning <- function(v) {
  # ngrukkon must be used on intervals not pitches, so warn based on a guess that the input might be pitch rather than interval values
  if(mean(v, na.rm = TRUE) > 20) warning("Are you definitely using intervals for ngrukkon?")
}
#' Score using the opti3 measure of similarity
#'
#' @param pitch_vec1
#' @param onset_vec1
#' @param pitch_vec2
#' @param onset_vec2
#' @param N
#' @param use_bootstrap
#' @param return_components
#' @param segmentation1
#' @param segmentation2
#'
#' @return
#' @export
#'
#' @examples
opti3 <- function(pitch_vec1, onset_vec1,
                  pitch_vec2, onset_vec2,
                  N = 3,
                  use_bootstrap = TRUE,
                  return_components = FALSE,
                  segmentation1 = NULL, segmentation2 = NULL) {

  warning('It is recommended to use the more comprehensive opti3_df version of opti3')

  stopifnot(all(c(pitch_vec1, pitch_vec2) > 0L))

  pitch_vec1 <- round(pitch_vec1)
  pitch_vec2 <- round(pitch_vec2)
  v_ngrukkon <- ngrukkon_safe(diff(pitch_vec1), diff(pitch_vec2), N = N)

  ioi1 <- c(NA, diff(onset_vec1))
  ioi2 <- c(NA, diff(onset_vec2))

  ioi_class1 <- classify_duration(ioi1)
  ioi_class2 <- classify_duration(ioi2)

  v_rhythfuzz <- rhythfuzz(ioi_class1, ioi_class2)

  if(use_bootstrap) {
    v_harmcore <- harmcore2(pitch_vec1, pitch_vec2)
  }
  else{
    v_harmcore <- harmcore(pitch_vec1, pitch_vec2)

  }
  opti3 <- 0.505 *  v_ngrukkon + 0.417  * v_rhythfuzz + 0.24  * v_harmcore - 0.146

  #messagef("ngrukkon = %.3f, rhythfuzz = %.3f, harmcor = %.3f, opti3 = %.3f",
  #         v_ngrukkon, v_rhythfuzz, v_harmcore, opti3)

  opti3 <- max(min(opti3, 1), 0)

  if(return_components) {
    list("opti3" = opti3,
         "ngrukkon" = v_ngrukkon,
         "rhythfuzz" = v_rhythfuzz,
         "harmcore" = v_harmcore)
  } else {
    opti3
  }

}


#' read a pYIN note track (outputted from Sonic Annotator or Tony) and make it nice
#'
#' @param fname
#' @param style
#'
#' @return
#' @export
#'
#' @examples
read_melody <- function(fname, style = c("sonic_annotator", "tony")) {
  warning("Have you specified whether it is a Sonic Annotator vs. Tony pitch track correctly?")
  melody <-
    read.csv(fname, header = FALSE) %>%
    tidyr::as_tibble() %>%
    {if(style == "sonic_annotator") dplyr::rename(., onset = V1, freq = V3, dur = V2) else dplyr::rename(.,onset = V1, freq = V2, dur = V3)} %>%
    itembankr::produce_extra_melodic_features() ## NB! sonic annotator and tony output different column orders, hence the above

  if(any(is.na(melody$note)) || any(is.infinite(melody$note))){
    stop("Warning: Melody (%s) contains invalid pitches", fname)
  }
  if(any(melody$ioi[!is.na(melody$ioi)] < .01)){
    stop("Warnings: Melody (%s) contains IOIs less than 10 ms, possibly no note track", fname)
  }
  melody
}




#' opti3 for melodies read by read_melody
#' returns sorted tibble of transpositions of melody2 and opti3 similarity
#'
#' @param melody1
#' @param melody2
#' @param N
#' @param use_bootstrap
#' @param return_winner
#'
#' @return
#' @export
#'
#' @examples
opti3_df <- function(melody1, melody2, N = 3, use_bootstrap = FALSE, return_winner = TRUE){
  trans_hints <- get_transposition_hints(melody1$note, melody2$note)
  v_rhythfuzz <- rhythfuzz(melody1$ioi_class, melody2$ioi_class)
  v_ngrukkon <- ngrukkon_safe(diff(melody1$note), diff(melody2$note), N = N)

  sims <- purrr::map_dfr(trans_hints, function(th){

    if(use_bootstrap) {
      v_harmcore <- harmcore2(melody1$note, melody2$note + th, segmentation1 = melody1$phrasbeg, segmentation2 = melody2$phrasbeg)
    }
    else{
      v_harmcore <- harmcore(melody1$note, melody2$note + th, segmentation1 = melody1$phrasbeg, segmentation2 = melody2$phrasbeg)

    }

    opti3 <-  0.505 *  v_ngrukkon + 0.417  * v_rhythfuzz + 0.24  * v_harmcore - 0.146
    opti3 <- max(min(opti3, 1), 0)

    tibble::tibble(transposition = th,
                   ngrukkon = v_ngrukkon,
                   rhythfuzz = v_rhythfuzz,
                   harmcore = v_harmcore,
                   opti3 = opti3)
  })
  res <- sims %>% dplyr::arrange(dplyr::desc(opti3))
  if(return_winner) {
    res %>% dplyr::slice(1)
  } else {
    res
  }
}

