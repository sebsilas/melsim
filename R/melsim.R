#' Compute the melodic similarity between melodies
#'
#' @param melody1 Vector of melody objects or file names - for comparison.
#' @param melody2 Vector of melody objects or file names - for comparison.
#' @param sim_measures Which similarity algorithm(s) to use.
#' @param paired
#' @param verbose
#' @param with_progress
#' @param with_checks
#' @param name
#'
#' @return
#' @export
#'
#' @examples
melsim <- function(melody1,
                   melody2 = NULL,
                   sim_measures = melsim::similarity_measures$opti3,
                   paired = FALSE,
                   verbose = TRUE,
                   with_progress = TRUE,
                   with_checks = TRUE,
                   name = "--") {

  # For now, only allow an actual sim measure to be passed.

  if(is.character(sim_measures)) {
    sim_measures <- purrr::map(sim_measures, function(sm) {
      melsim::similarity_measures[[sm]]
    })
  }

  # Add support for character vectors later

  if(!is.list(sim_measures)) {
    sim_measures <- list(sim_measures)
  }

  if(any(!sapply(sim_measures, is_sim_measure))){
    logging::logerror(sprintf("Received non-similarity measure"))
    browser()
    stop()
  }
  if(paired) assertthat::assert_that(!is.null(melody2), msg = "If paired is TRUE, melody2 must be non-NULL.")

  # Instantiate melodies

  if(is.null(melody2)){
    self_sim <- TRUE
    melody2 <- melody1
  } else {
    self_sim <- FALSE
  }

  if(with_checks) {
    conduct_checks(melody1, melody2)
  }

  # Make sure melody and similarity measure objects are lists before we iterate over them
  if(!is.list(melody1)){
    melody1 <- list(melody1)
  }
  if(!is.list(melody2)){
    melody2 <- list(melody2)
  }
  # Apply similarity algorithm(s)
  ret <-
    map_dfr(sim_measures, function(sim_algo) {

      # Verbose
      handle_verbose(verbose, sim_algo)

      # Progress
      show_progress(with_progress, paired, melody1, melody2)


      purrr::imap_dfr(unname(melody1), function(m1, i) {
        # Note that if melody1 is named, i will be a name, but we want an index, hence the unname()
        if(with_progress) cli::cli_progress_update(.envir = globalenv())
        #browser()

        if(!("name" %in% names(m1$meta))) {
          m1$add_meta("name", sprintf("SET1MEL%04d", i))
        }

        purrr::imap_dfr(unname(melody2), function(m2, j) {
          # Note that if melody2 is named, j will be a name, but we want an index, hence the unname()
          #browser()
          if(paired && i != j) {
            return(NULL)
          }
          if(!("name" %in% names(m2$meta)) && !self_sim) {
            m2$add_meta("name", sprintf("SET2MEL%04d", j))
          }

          if(self_sim && j<= i) {
            return(handle_self_sim(m1, m2, i, j, sim_algo))
          }

          if(is_sim_measure(sim_algo)) {
            sim <- m1$similarity(m2, sim_algo)
          } else {
            stop(sprintf("Similarity algorithm: '%s' not recognized.", as.character(sim_algo)))
          }
          tibble(melody1 = m1$meta$name,
                 melody2 = m2$meta$name) %>% bind_cols(sim)
        })

      })

    })
  #browser()
  ret <- ret %>%
    arrange(algorithm, melody1, melody2)

  if(with_progress) cli::cli_progress_done()

  ret <- sim_mat_factory$new(ret, name = name, paired = paired)

  if(self_sim){
    ret <- ret$make_symmetric()
  }
  ret
}

conduct_checks <- function(melody1, melody2) {
  if(any(!is_class(melody1, "Melody"))) {
    if(all(is_class(melody1, "character"))) {
      melody1 <- lapply(melody1, function(f) melody_factory$new(fname = f)$add_meta("name", f))
    } else {
      logging::logerror("Melody1 must be vector of melody objects or valid filenames")
      return(NULL)
    }
  }
  if(any(!is_class(melody2, "Melody"))) {
    if(all(is_class(melody2, "character"))) {
      melody2 <- lapply(melody2, function(f) melody_factory$new(fname = f))
    } else{
      logging::logerror("Melody2 must be vector of melody objects or valid filenames")
      return(NULL)
    }
  }
  # sim_algo_strs <- similarity_measures[is.character(similarity_measures)]
  # if(!all(is_official_sim_measure(sim_algo_strs))){
  #   sim_algo_str <- paste(sim_algo_strs[!is_official_sim_measure((sim_algo_strs))], collapse = ", ")
  #   logging::logerror(sprintf("Unrecognized similarity measures: '%s', bailing out.", sim_algo_str))
  #   return(NULL)
  # }
}


handle_verbose <- function(verbose, sim_algo) {
  if(verbose) {
    logging::loginfo(sprintf("Calculating: %s",
                             ifelse(is_sim_measure(sim_algo),
                                    sim_algo$name,
                                    sim_algo)))
  }
}


show_progress <- function(with_progress, paired, melody1, melody2) {
  if(with_progress) {
    total_length <- ifelse(paired, length(melody1), length(melody1) * (length(melody2) - 1)/2)
    cli::cli_progress_bar("Calculating similarities", total = length(melody1), .envir = globalenv())
  }
}

handle_self_sim <- function(m1, m2, i, j, sim_algo) {
  if(i == j) {
    # If sim_algo is a linear combination and we keep single we have to add all of them
    terms <- sim_algo$name

    if(!is.null(sim_algo$parameters$keep_singles) && sim_algo$parameters$keep_singles) {
      terms <- c(terms, parse_linear_combination(sim_algo$sim_measure) %>% pull(terms))
    }

    ret <- map_dfr(terms, function(t) {
      algorithm <- sim_algo$name %>% as.vector()
      full_name <- names(sim_algo$name)

      if(t %in% names(melsim::similarity_measures)){
        algorithm <- melsim::similarity_measures[[t]]$name
        full_name <-  melsim::similarity_measures[[t]]$full_name
      }

      tibble(melody1 = m1$meta$name,
             melody2 = m2$meta$name,
             algorithm = algorithm,
             full_name = full_name,
             sim = 1.0)
    })
    return(ret)
  } else if (j < i) {
    return(NULL)
  }
}

