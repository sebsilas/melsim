

optim_transposer <- function(query,
                             target,
                             sim_measure = edit_sim_utf8,
                             optimizer_pars = list(strategy = c("all", "hints", "best") ),
                             ...) {

  # Run for all transpositions and pick the top

  #query <- as.integer(query - median(query))
  #target <- as.integer(target - median(target))
  query <- query + 60 - min(c(query))
  target <- target + 60 - min(c(target))
  d <- stats::median(target)  -  stats::median(query)
  if(optimizer_pars$strategy == "all") {
    hints <- -5:6
  } else if(optimizer_pars$strategy == "hints" ) {
    hints <- get_transposition_hints(target, query )
  } else if(optimizer_pars$strategy == "best" ) {
    hints <- find_best_transposition(target, query)
  }
  purrr::map_dfr(union(d, hints), function(trans) {
    tibble(trans = trans, sim = sim_measure(query + trans, target))
  }) %>%
    slice_max(sim) %>% distinct(sim) %>% pull(sim)


}

optim_transposer_emd <- function(mel1,
                                 mel2,
                                 sim_measure = sim_emd,
                                 optimizer_pars = list(
                                   beta = .5,
                                   strategy = c("all", "hints", "best")
                                  ), ...) {

  #query <- as.integer(query - median(query))
  #target <- as.integer(target - median(target))
  mel1$pitch <- mel1$pitch + 60 - min(c(mel1$pitch))
  mel2$pitch <- mel2$pitch + 60 - min(c(mel2$pitch))
  d <- stats::median(mel2$pitch)  -  stats::median(mel1$pitch)
  if(optimizer_pars$strategy == "all") {
    hints <- -5:6
  } else if(optimizer_pars$strategy == "hints") {
    hints <- get_transposition_hints(mel2$pitch, mel1$pitch )
  } else if(optimizer_pars$strategy == "best") {
    hints <- find_best_transposition(mel2$pitch, mel1$pitch)
  }
  ret <- purrr::map_dfr(union(d, hints), function(trans) {
    tibble(trans = trans, sim = sim_measure(mel1 %>% mutate(pitch = pitch + trans), mel2))
  }) %>%
    slice_max(sim) %>% distinct(sim) %>% pull(sim)


}


optim_slide_shorter_melody <- function(query,
                                       target,
                                       sim_measure = edit_sim_utf8,
                                       pars = NULL,
                                       ...) {
  vec_type <- class(query)
  query <- na.omit(query)
  target <- na.omit(target)
  if(vec_type == "factor"){
    query <- as.character(query)
    target <- as.character(target)
    vec_type <- "character"
  }
  query_length <- length(query)
  target_length <- length(target)
  #browser()

  if(query_length == target_length) {
    return(sim_measure(query, target))
  } else if (query_length < target_length) {
    shorter_melody <- query
    longer_melody_ngrams <- get_all_ngrams(target, N = query_length)
  } else if(query_length > target_length) {
    shorter_melody <- target
    longer_melody_ngrams <- get_all_ngrams(query, N = target_length)
  }
  logging::loginfo(sprintf("Sliding on..."))
  purrr::pmap_dfr(longer_melody_ngrams, function(start, N, value) {
    ngram <- value_to_vec(value, type = vec_type, collapse = ",")
    tibble(ngram_pos = start,
           sim = sim_measure(ngram, shorter_melody))
  }) %>%
    slice_max(sim) %>%
    distinct(sim) %>%
    pull(sim)

}


apply_optimizer <- function(optimizer_name = optimizers,
                            query,
                            target,
                            sim_measure = edit_sim_utf8,
                            optimizer_pars,
                            ...) {

  optimizer_name <- match.arg(optimizer_name)

  switch(optimizer_name,
         optimizer_emd = optim_transposer_emd(query, target, sim_measure, optimizer_pars, ...),
         transpose = optim_transposer(query, target, sim_measure, optimizer_pars, ...),
         slide_shorter_melody = optim_slide_shorter_melody(query, target, sim_measure, optimizer_pars, ...),
         stop("Invalid `optimizer_name` value")
         )

}

optimizers <- c("optimizer_emd", "transpose", "slide_shorter_melody")
