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
                   similarity_measures = c("ngrukkon",
                                          "rhytfuzz",
                                          "harmcore"
                                          # "opti3",
                                          # "needleman_wunsch",
                                          # "compression_distance_gzip",
                                          # "correlation",
                   ),
                   paired = FALSE,
                   name = "--") {
  # Instantiate melodies
  #browser()
  if(any(!is_class(melody1, "Melody"))){
    if(all(is_class(melody1, "character"))){
      melody1 <- lapply(melody1, function(f) melody_factory$new(fname = f)$add_meta("name", f))
    }
    else{
      logging::lorerror("Melody1 must be vector of melody objects or valid filenames")
      return(NULL)
    }
  }
  self_sim <- FALSE
  if(is.null(melody2)){
    self_sim <- TRUE
    melody2 <- melody1
  }
  if(any(!is_class(melody2, "Melody"))){
    if(all(is_class(melody2, "character"))){
      melody2 <- lapply(melody2, function(f) melody_factory$new(fname = f))
    }
    else{
      logging::lorerror("Melody2 must be vector of melody objects or valid filenames")
      return(NULL)
    }
  }
  sim_algo_strs <- similarity_measures[is.character(similarity_measures)]
  if(!all(is_sim_measure(sim_algo_strs))){
    sim_algo_str <- paste(sim_algo_strs[!is_sim_measure((sim_algo_strs))], collapse = ", ")
    logging::logerror(sprintf("Unrecognized similarity measures: %s ", sim_algo_str))
    return(NULL)
  }
  #prepare for using, all must be lists
  if(all(is_class(similarity_measures, "SimilarityMeasure")) && !is.list(similarity_measures)){
    similarity_measures <- list(similarity_measures)
  }
  if(!is.list(melody1)){
    melody1 <- list(melody1)
  }
  if(!is.list(melody2)){
    melody2 <- list(melody2)
  }

  # Apply the similarity algorithm
  ret <-
    map_dfr(similarity_measures, function(sim_algo){
      #browser()
      logging::loginfo(sprintf("Testing: %s", ifelse(methods::is(sim_algo, "SimilarityMeasure"),
                                                     sim_algo$name, sim_algo)))

      purrr::imap_dfr(melody1, function(m1, i){
        #browser()
        if(!("name" %in% names(m1$meta))){
          m1$add_meta("name", sprintf("SET1MEL%04d", i))
        }
        purrr::imap_dfr(melody2, function(m2, j){
          if(paired && i != j){
            return(NULL)
          }
          if(!("name" %in% names(m2$meta)) && !self_sim){
            m2$add_meta("name", sprintf("SET2MEL%04d", j))
          }
          if(is.character(sim_algo)){
            sim_algo_str <- sim_algo
            sim_algo <- melsim::similarity_measures[[sim_algo_str]]
            if(is.null(sim_algo)){
              logging::logwarn(sprintf("Unrecognized similarity measure: %s ", sim_algo_str))
              return(NULL)
            }
          }
          if(self_sim){
            if(i == j){
              #if sim_alog is a linear combination and we keep single we have to add all of them
              terms <- sim_algo$name
              if(!is.null(sim_algo$parameters$keep_singles) && sim_algo$parameters$keep_singles){
                terms <- c(terms, parse_linear_combination(sim_algo$sim_measure) %>% pull(terms))

              }
              ret <- map_dfr(terms, function(t){
                tibble(melody1 = m1$meta$name,
                       melody2 = m2$meta$name,
                       algorithm = melsim::similarity_measures[[t]]$name,
                       full_name = melsim::similarity_measures[[t]]$full_name,
                       sim = 1.0)
              })
              return(ret)
            } else if (j < i){
              return(NULL)
            }}

          if(methods::is(sim_algo, "SimilarityMeasure")){
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

  ret <- sim_mat_factory$new(ret, name = name, paired = paired)
  if(self_sim){
    ret <- ret$make_symmetric()
  }
  ret
}


# t <- list.files('data-raw', full.names = TRUE) %>%
#   melsim_many_to_many_multiple_algorithms()

test_melsim <- function(N = 20){
  tictoc::tic()
  kinder_full <- update_melodies(kinder_full)
  ret <-
    melsim(
      #c('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv'),
      #melody1 = list.files("data-raw/kinder/", pattern = "csv", full.names = T),
      melody1 = kinder_full[sample(1:length(kinder_full), N)],
      melody2 = NULL,
      similarity_measures = c("opti3", "ncdintioi", "diffed")#, "pmi_ps",   "rhytfuzz", "diffed", "harmcore")
    )
  tictoc::toc()
  invisible(ret)
}
