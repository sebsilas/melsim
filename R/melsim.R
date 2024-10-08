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
                   similarity_measures = get_sim_measures(),
                   paired = FALSE,
                   verbose = T,
                   with_progress = TRUE,
                   with_checks = TRUE,
                   name = "--") {

  similarity_measures <- match.arg(similarity_measures)

  # Instantiate melodies

  self_sim <- FALSE

  if(is.null(melody2)){
    self_sim <- TRUE
    melody2 <- melody1
  }

  if(with_checks){
    if(any(!is_class(melody1, "Melody"))){
      if(all(is_class(melody1, "character"))){
        melody1 <- lapply(melody1, function(f) melody_factory$new(fname = f)$add_meta("name", f))
      }
      else{
        logging::logerror("Melody1 must be vector of melody objects or valid filenames")
        return(NULL)
      }
    }
    if(any(!is_class(melody2, "Melody"))){
      if(all(is_class(melody2, "character"))){
        melody2 <- lapply(melody2, function(f) melody_factory$new(fname = f))
      }
      else{
        logging::logerror("Melody2 must be vector of melody objects or valid filenames")
        return(NULL)
      }
    }
    sim_algo_strs <- similarity_measures[is.character(similarity_measures)]
    if(!all(is_sim_measure(sim_algo_strs))){
      sim_algo_str <- paste(sim_algo_strs[!is_sim_measure((sim_algo_strs))], collapse = ", ")
      logging::logerror(sprintf("Unrecognized similarity measures: '%s', bailing out.", sim_algo_str))
      return(NULL)
    }
  }
  sim_measures <- similarity_measures
  sim_measures[is.character(sim_measures)] <- melsim::similarity_measures[sim_measures[is.character(sim_measures)]]
  sim_measures <- sim_measures[!is.null(sim_measures)]
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
    map_dfr(sim_measures, function(sim_algo){
      #browser()
      if(verbose) {
        logging::loginfo(sprintf("Calculating: %s",
                                 ifelse(methods::is(sim_algo, "SimilarityMeasure"),
                                        sim_algo$name,
                                        sim_algo)))
      }
      if(with_progress) {
        total_length <- ifelse(paired, length(melody1), length(melody1) * (length(melody2) - 1)/2)
        cli::cli_progress_bar("Calculating similarities", total = length(melody1), .envir = globalenv())
      }
      purrr::imap_dfr(melody1, function(m1, i){
        if(with_progress) cli::cli_progress_update(.envir = globalenv())
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

  if(with_progress) cli::cli_progress_done()

  ret <- sim_mat_factory$new(ret, name = name, paired = paired)
  if(self_sim){
    ret <- ret$make_symmetric()
  }
  ret
}



test_melsim <- function(N = 20, sim_measure = c("ngrukkon")){
  tictoc::tic()
  kinder_full <- update_melodies(kinder_full, force = TRUE)
  ret <-
    melsim(
      #c('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv'),
      #melody1 = list.files("data-raw/kinder/", pattern = "csv", full.names = T),
      #melody1 = kinder_full[sample(1:length(kinder_full), N)],
      melody1 = kinder_full[1:N],
      melody2 = NULL,
      similarity_measures = sim_measure#, "pmi_ps",   "rhytfuzz", "diffed", "harmcore")
    )
  tictoc::toc()
  invisible(ret)
}

test_dtw <- function(N = 20){
  tictoc::tic()
  kinder_full <- update_melodies(kinder_full, force = T)
  ret <-
    map_dfr(1:N, function(i){
    map_dfr(1:N, function(j){
      if(j <= i){
        return(NULL)
      }
      DTW <- dtw::dtw(kinder_full[[i]]$data$onset, kinder_full[[j]]$data$onset)
      tibble(i = i, j = j,
             l1 = kinder_full[[i]]$length, l2 = kinder_full[[j]]$length,
             dist = DTW$distance,
             normed_dist = DTW$normalizedDistance,
             N = DTW$N,
             M = DTW$M,
             steps = sum(DTW$stepsTaken))
    })
  })
  tictoc::toc()
  invisible(ret)
}
