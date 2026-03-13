

optim_transposer <- function(query,
                             target,
                             sim_measure = edit_sim_utf8,
                             parameters = list(strategy = c("all", "hints", "best") ),
                             ...) {

  if(is.null(parameters)){
    parameters <- list(strategy = "all")
  }
  parameters$strategy <- parameters$strategy[1]


  query <- query + 60 - min(c(query))
  target <- target + 60 - min(c(target))

  d <- stats::median(target)  -  stats::median(query)

  if(parameters$strategy == "all") {
    hints <- -5:6
  } else if(parameters$strategy == "hints" ) {
    hints <- get_transposition_hints(target, query )
  } else if(parameters$strategy == "best" ) {
    hints <- find_best_transposition(target, query)
  }
  #browser()
  # Run for all transpositions and pick the top
  ret <- purrr::map_dfr(union(d, hints), function(trans) {
    tibble(trans = trans, sim = sim_measure(query + trans, target))
  })
  ret %>% pull(sim) %>% max()
}

optim_transposer_emd <- function(mel1,
                                 mel2,
                                 sim_measure = sim_emd,
                                 parameters = list(
                                   beta = .5,
                                   strategy = c("all", "hints", "best")
                                  ), ...) {

  mel1$pitch <- mel1$pitch + 60 - min(c(mel1$pitch))
  mel2$pitch <- mel2$pitch + 60 - min(c(mel2$pitch))

  if(is.null(parameters)){
    parameters <- list(strategy = "all", beta = .5)
  }
  parameters$strategy <- parameters$strategy[1]

  d <- stats::median(mel2$pitch)  -  stats::median(mel1$pitch)

  if(parameters$strategy == "all") {
    hints <- -5:6
  }
  else if(parameters$strategy == "hints") {
    hints <- get_transposition_hints(mel2$pitch, mel1$pitch )
  }
  else if(parameters$strategy == "best") {
    hints <- find_best_transposition(mel2$pitch, mel1$pitch)
  }
  ret <- purrr::map_dfr(union(d, hints), function(trans) {
    tibble(trans = trans,
           sim = sim_measure(mel1 %>% mutate(pitch = pitch + trans),
                             mel2))
  }) %>% pull(sim) %>% max()


}

optim_transposer_ih <- function(mel1,
                                 mel2,
                                 sim_measure = edit_sim_utf8,
                                 parameters = list(
                                   strategy = c("all", "hints", "best")
                                 ),
                                ...) {
  mel1$pitch <- mel1$pitch + 60 - min(c(mel1$pitch))
  mel2$pitch <- mel2$pitch + 60 - min(c(mel2$pitch))
  if(is.null(parameters)){
    parameters <- list(strategy = "all")
  }
  strategy <- parameters$strategy[1]

  d <- stats::median(mel2$pitch)  -  stats::median(mel1$pitch)

  if(strategy == "all") {
    hints <- 0:11
  }
  else if(strategy == "hints") {
    hints <- get_transposition_hints(mel2$pitch, mel1$pitch )
  }
  else if(strategy == "best") {
    hints <- find_best_transposition(mel2$pitch, mel1$pitch)
  }

  ih1 <- get_implicit_harmonies(mel1$pitch, segmentation = mel1$bar)
  ih2 <- get_implicit_harmonies(mel2$pitch, segmentation = mel2$bar)%>%
    mutate(symbols = key_pc  + 12 * as.integer(factor(type))- 12) %>%
    pull(symbols)

  ret <- purrr::map_dfr(union(d, hints) %% 12, function(trans) {
    #browser()
    ih1_trans <- (ih1$key_pc + trans) %% 12 + 12 * as.integer(factor(ih1$type))- 12
    tibble(trans = trans,
           sim = sim_measure(ih1_trans, ih2))
  })
  #browser()
  ret %>% pull(sim) %>% max()
}

optim_slide_shorter_melody <- function(query,
                                       target,
                                       sim_measure = edit_sim_utf8,
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
  purrr::pmap_dfr(longer_melody_ngrams, function(start, N, value) {
    ngram <- value_to_vec(value, type = vec_type, collapse = ",")
    tibble(ngram_pos = start,
           sim = sim_measure(ngram, shorter_melody))
  }) %>%
    pull(sim) %>%
    max()

}


apply_optimizer <- function(query,
                            target,
                            sim_measure = edit_sim_utf8,
                            name = optimizers,
                            parameters,
                            ...) {
  # if(is.null(name) ||
  #    !is.character(name) ||
  #    nchar(name) == 0){
  #   name <- "none"
  # }
  name <- match.arg(name)
  switch(name,
         none = sim_measure(query, target),
         optimizer_emd = optim_transposer_emd(query, target, sim_measure, parameters, ...),
         transpose = optim_transposer(query, target, sim_measure, parameters, ...),
         slide_shorter_melody = optim_slide_shorter_melody(query, target, sim_measure,  ...),
         stop(sprintf("Invalid optimizer; `%s` ", name))
         )

}

optimizers <- c("none", "optimizer_emd", "transpose", "slide_shorter_melody")
