

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
                                  limit_corpus_N = NULL,
                                  return_sim_values = FALSE,
                                  plot = FALSE) {

  stopifnot(is.null.or(limit_corpus_N, is.integer))
  if(plot){
    return_sim_values <- T
  }
  if(is.numeric(limit_corpus_N)) {
    benchmark_corpus <- benchmark_corpus[1:limit_corpus_N]
  }

  benchmark_corpus <- update_melodies(benchmark_corpus, force = TRUE)


  ret <- purrr::map_dfr(sim_measures, function(sm) {

    tictoc::tic()

    sim_res <-
      melsim(
        melody1 = benchmark_corpus,
        sim_measures =  sm
      )

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

benchmark_plot <- function(dat, by_measure = TRUE) {

  data_combined <- pmap_dfr(dat, function(...) {
    d <- list(...)

    sim_measure_name <- unique(d$sim_measure_name)

    d$sim_measure_name <- NULL

    d <- d$data  %>%
      filter(melody1 < melody2) %>%
      dplyr::select(-c(sim_measure_name, melody1, melody2))

    names(d) <- "sim_value"


    d %>%
      dplyr::mutate(`Similarity Measure` = sim_measure_name )

  })

  if(by_measure) {
    p <- data_combined %>%
      ggplot(aes(x = sim_value, fill = `Similarity Measure`)) +
      geom_histogram(color = "black") +
      facet_wrap(~`Similarity Measure`) +
      labs(x = "Similarity", y = "Count") +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal()
  } else {
    p <- data_combined %>%
      ggplot(aes(x = sim_value)) +
      geom_histogram(color = "black") +
      labs(x = "Similarity", y = "Count") +
      scale_fill_brewer(palette = "Set1") +
      theme_minimal()
  }


  print(p)
}

