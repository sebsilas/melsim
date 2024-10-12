library(tidyverse)

test_melsim <- function(N = 20, sim_measure = c("ngrukkon", "diffed", "rawed", "diffsd", "rawsd")){
  tictoc::tic()
  kinder_full <- update_melodies(kinder_full, force = TRUE)
  ret <-
    melsim(
      #c('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv'),
      #melody1 = list.files("data-raw/kinder/", pattern = "csv", full.names = T),
      #melody1 = kinder_full[sample(1:length(kinder_full), N)],
      melody1 = kinder_full[1:N],
      melody2 = NULL,
      similarity_measures = similarity_measures[sim_measure]#, "pmi_ps",   "rhytfuzz", "diffed", "harmcore")
    )
  tictoc::toc()
  invisible(ret)
}

test_dtw <- function(N = 20){
  tictoc::tic()
  kinder_full <- update_melodies(kinder_full, force = TRUE)
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
sim_grids <- function(){
  ngram_measures <- c("ukkon", "sum_common", "count_distinct", "dist_sim", "Tversky")

  measures_with_extra_pars <- c(ngram_measures,
                                # "Tversky", For now we leave this out parameter exploration and use auto + the Berkowitz db
                                "Minkowski",
                                "pmi")


  similarity_grid_base <- expand_grid(sim_transformation = setdiff(sim_transformations,
                                                                   c("ngrams", "none")),
                                      sim_measure = setdiff(low_level_sim_measures,
                                                            measures_with_extra_pars))   %>%
    rowwise() %>%
    mutate(
      transposition_invariant = F,#is_transposition_invariant(sim_transformation),
      tempo_invariant = F,#melsim:::is_tempo_invariant(sim_transformation),
      sim_type = melsim:::get_sim_type(sim_measure),
      ngram_transformation = NA,
      ngram_length = NA,
      p_minkowski = NA,
      transpose_optimizer = NA,
      sim_name = paste0(
        paste0(c(sim_transformation,
                 sim_measure), collapse = "-"),
        "_transposition_invariant=", transposition_invariant,
        "_tempo_invariant=", tempo_invariant)
    ) %>%
    ungroup()
  similarity_grid_ngram <- expand_grid(sim_transformation = "ngrams",
                                       sim_measure = set_based_measures,
                                       ngram_length = 1:3,
                                       ngram_transformation = setdiff(sim_transformations,
                                                                      c("none", "ngrams"))) %>%
    rowwise() %>%
    mutate(
      transposition_invariant = melsim:::is_transposition_invariant(sim_transformation),
      tempo_invariant = melsim:::is_tempo_invariant(sim_transformation),
      sim_type = melsim:::get_sim_type(sim_measure),
      sim_name = paste0(
        paste0(c(sim_transformation,
                 sim_measure), collapse = "-"),
        "_ngram_transformation=", ngram_transformation,
        "_ngram_length=", ngram_length),
      p_minkowski = NA,
      transpose_optimizer = NA,
    ) %>%
    ungroup() %>%
    relocate(sim_transformation, sim_measure, transposition_invariant,
             tempo_invariant, sim_type, ngram_transformation, ngram_length, p_minkowski, transpose_optimizer, sim_name)


  similarity_grid_minkowski <- expand_grid(sim_transformation = setdiff(sim_transformations,
                                                                        c("ngrams", "none")),
                                           p_minkowski = c(0.5, 1, 2, 3, Inf)) %>%
    rowwise() %>%
    mutate(
      sim_measure = "Minkowski",
      transposition_invariant = melsim:::is_transposition_invariant(sim_transformation),
      tempo_invariant = melsim:::is_tempo_invariant(sim_transformation),
      sim_type = melsim:::get_sim_type(sim_measure),
      sim_name = paste0(
        paste0(c(sim_transformation,
                 sim_measure), collapse = "-"),
        "_p=", p_minkowski),
      ngram_transformation = NA,
      ngram_length = NA,
      transpose_optimizer = NA
    ) %>%
    ungroup() %>%
    relocate(sim_transformation, sim_measure, transposition_invariant,
             tempo_invariant, sim_type, ngram_transformation, ngram_length, p_minkowski, transpose_optimizer, sim_name)


  similarity_grid_pmi <- expand_grid(sim_transformation = "pitch",
                                     transpose_optimizer = c(TRUE, FALSE)) %>%
    rowwise() %>%
    mutate(
      sim_measure = "pmi",
      transposition_invariant = melsim:::is_transposition_invariant(sim_transformation, if(transpose_optimizer) "transpose" else NULL),
      tempo_invariant = melsim:::is_tempo_invariant(sim_transformation),
      sim_type = melsim:::get_sim_type(sim_measure),
      sim_name = paste0(
        paste0(c(sim_transformation,
                 sim_measure), collapse = "-"),
        "_transpose_optimizer=", transpose_optimizer),
      ngram_transformation = NA,
      ngram_length = NA,
      p_minkowski = NA
    ) %>%
    ungroup() %>%
    relocate(sim_transformation, sim_measure, transposition_invariant,
             tempo_invariant, sim_type, ngram_transformation, ngram_length, p_minkowski, transpose_optimizer, sim_name)


  similarity_grid <- rbind(similarity_grid_base,
                           similarity_grid_ngram,
                           similarity_grid_minkowski,
                           similarity_grid_pmi)

  similarity_grid
}

similarity_grid <- sim_grids()
all_sims_test <- function(){
sim_test <- similarity_grid %>%
  #filter(sim_measure == "Yule2", sim_transformation == "int") %>%
  mutate(id = row_number() ) %>%
  pmap_dfr(function(sim_transformation,
                    sim_measure,
                    transposition_invariant,
                    tempo_invariant,
                    sim_type,
                    ngram_transformation,
                    ngram_length,
                    p_minkowski,
                    transpose_optimizer,
                    sim_name,
                    id) {


    logging::loginfo("[%d] Percent complete %s %%", id, round((id/nrow(similarity_grid) * 100)))
    if(sim_transformation == "ngrams" && sim_type == "metric"){
      browser()
      return(NULL)
    }
    pars <- list()
    if(sim_measure == "pmi" && !is.na(transpose_optimizer)) {
      if(transpose_optimizer) {
        pars <- list(optimizer = "transpose")
      }
    }

    if(sim_measure == "Tversky") {

      pars <- list(ngram_db = "melsim::int_ngrams_berkowitz",
                   alpha = "auto",
                   beta = "auto",
                   transformation = ngram_transformation,
                   ngram_length = ngram_length)

    }

    if(sim_measure == "sim_emd") {
      pars <- list(beta = .5,
                   optimizer = "transpose",
                   strategy = "all")
    }

    if(sim_transformation == "ngrams" && sim_measure != "Tversky") {

      pars <- list(transformation = ngram_transformation,
                   ngram_length = ngram_length)
    }

    if(sim_measure == "Minkowski") {
      pars <- list(p = p_minkowski)
    }


    beatles12 <- update_melodies(beatles[1:2], force = T)
    tictoc::tic("Time sim measure")

    if(sim_measure == "Mahalanobis") {
      # Can't tolerate a singular matrix,
      # so won't work for this test with the same melody for comparison
      res <-
        tibble::tibble(melody1 = beatles12[[1]]$meta$name,
                       melody2 = beatles12[[2]]$meta$name,
                       algorithm = sim_measure,
                       sim = NA)
    } else {

      #print('sim_measure')
      #print(sim_measure)
      #print('pars')
      pars_string <- ifelse(length(pars) > 0, sprintf("%s: %s", names(pars), pars), "<no params>")
      sim_name <- sprintf("%s_%s", sim_transformation, sim_measure)
      logging::loginfo("Testing: %s (params: %s)", sim_name, pars_string)
      sim_measure <- sim_measure_factory$new(full_name = sim_name,
                                             name = abbreviate(sim_name),
                                             transformation = sim_transformation,
                                             parameters = pars,
                                             sim_measure = sim_measure)

      res <- melsim(
        melody1 = beatles12[[1]],
        melody2 = beatles12[[2]],
        similarity_measures = sim_measure,
        paired = TRUE,
        verbose = TRUE,
        with_checks = FALSE,
        with_progress = TRUE,
        name = sim_name
      )
      res <- res$data %>%
        select(melody1, melody2, algorithm, sim)
    }


    finish <- tictoc::toc()
    time_taken_seconds <- as.numeric(finish$toc - finish$tic)

    res <- res %>%
      mutate(error = FALSE,
             time_taken = time_taken_seconds)


    return(res)

  })
  sim_test
}


#
#
# # test_proxy_pkg <- function() {
# #   #################################
# #   ## Test for dist calls
# #   ##################################
#
# library(proxy)
#
# set.seed(20140107)
#
# ## get all measures
# proxies = pr_DB$get_entry_names()
#
# ## remove special cases
# proxies = setdiff(proxies, c("Mahalanobis", "Minkowski", "Stiles", "Levenshtein", "fJaccard"))
#
# ## create test data
# #x = matrix(1:100, 10)
#
#
#
# ## test function: checks if dist(x) == dist(x,x) for all measures,
# ## and if diag(dist(x, x)) == diag(x, x, pairwise = TRUE)
# prtest <- function(...) {
#   CD <- dist(x, x, ...)
#   all(as.matrix(dist(x, ...)) == CD) &&
#     all(diag(CD) == dist(x, x, pairwise = TRUE, ...))
# }
#
# ## loop over all measures (except special cases)
# for (i in proxies)
# {cat(i); prtest(i); cat(": OK.\n")}
#
#
# ## Minkowski
# for (j in c(0.5, 1, 2, 3, Inf))
# {cat("Minkowski: p =", j); prtest("Minkowski", p = j); cat(": OK.\n")}
#
# ## Mahalanobis (need non-singular matrix)
# x = as.matrix(iris[1:50,-5])
# prtest("Mahalanobis")
#
# ## fJaccard (needs values in unit interval)
# x = as.matrix(1:100/100, 10)
# prtest("fJaccard")
#
# ## produce binary matrix
# x = matrix(rbinom(100,1,0.7), 10)
#
# ## Stiles (gives a lot of warnings due to log)
# tmp = dist(x, "Stiles")
# tmp = dist(x, x, "Stiles")
#
# ## try again (almost) all measures, this time with binary data to check
# ## conversions
# for (i in proxies)
# {cat(i); prtest(i); cat(": OK.\n")}
# ## Minkowski
# for (j in c(0.5, 1, 2, 3, Inf))
# {cat("Minkowski: p =", j); prtest("Minkowski", p = j); cat(": OK.\n")}
#
# ## Levenshtein distance
# s <- c("A", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog")
# all(as.matrix(dist(s, "Levenshtein")) == dist(s, s, "Levenshtein"))
#
# ## Test auto-conversion
# x = iris[,-5]
# prtest()
#
# #}
