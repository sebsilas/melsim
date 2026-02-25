

#' Benchmark similarity measures
#'
#' @param sim_measures
#' @param benchmark_corpus
#' @param limit_corpus_N
#' @param return_sim_values
#' @param plot
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
#' ex1 <- benchmark_sim_measure(limit_corpus_N = 20L)
#'
#' ex2 <- benchmark_sim_measure(limit_corpus_N = 20L, return_sim_values = TRUE, plot = TRUE)
#'
#'
# return_data <- benchmark_sim_measure(limit_corpus_N = 20L,
#                                      return_sim_values = TRUE,
#                                      plot = FALSE,
#                                      sim_measures = c("ngrukkon", "diffed", "rawed", "opti3", "pmips",
#                                                       "ncdintioi", "emd", "dtw", "tanimoto", "ngram_int_tversky_auto_int_2"))
#'
# return_data %>%
#   benchmark_plot()
#'
benchmark_sim_measure <- function(sim_measures = c("ngrukkon", "diffed", "rawed"),
                                  benchmark_corpus = kinder_full,
                                  benchmark_corpus_query = NULL,
                                  limit_corpus_N = NULL,
                                  return_sim_values = FALSE,
                                  plot = FALSE) {

  stopifnot(is.null.or(limit_corpus_N, is.integer))

  if(is.integer(limit_corpus_N)) {
    benchmark_corpus <- benchmark_corpus[1:limit_corpus_N]
  }

  benchmark_corpus <- update_melodies(benchmark_corpus, force = TRUE)

  if(!is.null(benchmark_corpus_query)) {
    if(is.integer(limit_corpus_N)) {
      benchmark_corpus_query <- benchmark_corpus_query[1:limit_corpus_N]
    }
    benchmark_corpus_query <- update_melodies(benchmark_corpus_query, force = TRUE)
  }

  if(!is.list(sim_measures)) {
    sim_measures <- list(sim_measures)
  }


  ret <- purrr::map_dfr(sim_measures, function(sm) {

    tictoc::tic()

    sim_res <-
      melsim(
        melody1 = benchmark_corpus,
        melody2 = benchmark_corpus_query,
        sim_measures =  sm
      )

    if(!is.character(sm)) {
      sm <- sm$name
    }

    dat <- sim_res$as_tibble() %>%
      tidyr::pivot_longer(dplyr::contains("sim_"), names_to = "sim_measure_name", values_to = "similarity") %>%
      dplyr::filter(sim_measure_name == !! paste0("sim_", sm) )

    summary_stats <- dat %>%
      group_by(sim_measure_name) %>%
      summarise(
        across(
          -c(melody1, melody2),
          list(
            n        = ~sum(!is.na(.)),
            mean     = ~mean(., na.rm = TRUE),
            sd       = ~sd(., na.rm = TRUE),
            median   = ~median(., na.rm = TRUE),
            min      = ~min(., na.rm = TRUE),
            max      = ~max(., na.rm = TRUE),
            IQR      = ~IQR(., na.rm = TRUE),
            skewness = ~e1071::skewness(., na.rm = TRUE, type = 2),
            kurtosis = ~e1071::kurtosis(., na.rm = TRUE, type = 2)
          ),
          .names = "{.fn}"
        )
      ) %>%
      dplyr::ungroup()

    time_taken <- as.numeric(tictoc::toc()$toc)

    summary_stats <- summary_stats %>%
      mutate(time_taken = time_taken) %>%
    dplyr::mutate( data = if(return_sim_values) list (dat) else  NULL )

  })

  if(plot) {
    benchmark_plot(ret)
  }

  return(ret)


}

#' Benchmark plot
#'
#' @param dat
#' @param by_measure
#'
#' @returns
#' @export
#'
#' @examples
benchmark_plot <- function(dat, by_measure = TRUE) {

  data_combined <- purrr::pmap_dfr(dat, function(...) {

    d <- list(...)

    sim_measure_name <- unique(d$sim_measure_name)

    d$sim_measure_name <- NULL

    d <- d$data %>%
      dplyr::select(-c(sim_measure_name, melody1, melody2))

    names(d) <- "sim_value"


    d %>%
      dplyr::mutate(`Similarity Measure` = sim_measure_name )

  })

  if(by_measure) {
    p <- data_combined %>%
      ggplot2::ggplot(ggplot2::aes(x = sim_value, fill = `Similarity Measure`)) +
      ggplot2::geom_histogram() +
      ggplot2::facet_wrap(~`Similarity Measure`) +
      ggplot2::labs(x = "Similarity", y = "Count")
  } else {
    p <- data_combined %>%
      ggplot2::ggplot(aes(x = sim_value)) +
      ggplot2::geom_histogram() +
      ggplot2::labs(x = "Similarity", y = "Count")
  }


  print(p)
}

