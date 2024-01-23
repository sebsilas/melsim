#' Compute the melodic similarity two melodies
#'
#' @param melody1 Vector of melody objects or file names - for comparison.
#' @param melody2 Vector of melody objects or file names - for comparison.
#' @param similarity_algorithm Which similarity algorithm to use.
#' @param ngram_length For set-based similarity, the N-gram length.
#'
#' @return
#' @export
#'
#' @examples
melsim <- function(melody1,
                   melody2 = NULL,
                   similarity_measures = c("ngram_ukkon",
                                          "rhythfuz",
                                          "harmcore"
                                          # "opti3",
                                          # "needleman_wunsch",
                                          # "compression_distance_gzip",
                                          # "correlation",
                   ),
                   name = "MELSIM") {
  # Instantiate melodies
  if(any(!is(melody1, "Melody"))){
    if(all(is.character(melody1))){
      melody1 <- lapply(melody1, function(f) melody_factory$new(fname = f)$add_meta("name", f))
    }
  }
  else{
    stop("Melody1 must be vector of melody objects of valid filenames")
  }
  self_sim <- FALSE
  if(is.null(melody2)){
    self_sim <- TRUE
    melody2 <- melody1
  }
  if(any(!is(melody2, "Melody"))){
    if(all(is.character(melody2))){
      melody2 <- lapply(melody2, function(f) melody_factory$new(fname = f))
    }
  }
  else{
    stop("Melody2 must be vector of melody objects or valid filenames")
  }
  # Apply the similarity algorithm
  ret <-
    map_dfr(similarity_measures, function(sim_algo){
      #browser()
      logging::loginfo(sprintf("Testing: %s", sim_algo))
      imap_dfr(melody1, function(m1, i){
        #browser()
        if(!("name" %in% names(m1$meta))){
          m1$add_meta("name", sprintf("SET1MEL%03d", i))
        }
        imap_dfr(melody2, function(m2, j){

          if(!("name" %in% names(m2$meta)) && !self_sim){
            m2$add_meta("name", sprintf("SET2MEL%03d", j))
          }
          if(is.character(sim_algo)){
            sim_algo_str <- sim_algo
            sim_algo <- melsim::similarity_measures[[sim_algo_str]]
            if(is.null(sim_algo)){
              logging::logwarn(sprintf("Unrecognized similarity measure: %s ", sim_algo_str))
            }
          }
          if(self_sim){
            if(i == j){
              return(tibble(melody1 = m1$meta$name,
                            melody2 = m2$meta$name,
                            algorithm = sim_algo$name,
                            full_name = sim_algo$full_name,
                            sim = 1.0))
            } else if (j < i){
              return(NULL)
            }}

          if(is(sim_algo, "SimilarityMeasure")){
            #tictoc::tic(msg = sim_algo$name)
            sim <- m1$similarity(m2, sim_algo)
            #tictoc::toc()
          }
          else{
            stop(sprintf("Similarity algorithm: '%s' not recognized.", as.character(sim_algo)))
          }
          tibble(melody1 = m1$meta$name,
                 melody2 = m2$meta$name) %>% bind_cols(sim)
        })
      })
    }) %>%
    arrange(algorithm, melody1, melody2)
  ret <- sim_mat_factory$new(ret, name = name)
  if(self_sim){

    ret <- ret$make_symmetric()
  }
  ret
}


# t <- list.files('data-raw', full.names = TRUE) %>%
#   melsim_many_to_many_multiple_algorithms()

test_melsim <- function(){
  melsim(
    #c('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv'),
    melody1 = list.files("data-raw/kinder", pattern = "csv", full.names = T),
    melody2 = NULL,
    similarity_measures = c("ngram_ukkon", "rhythfuz"))
  #melsim('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv', similarity_algorithm = 'set_based')

}
