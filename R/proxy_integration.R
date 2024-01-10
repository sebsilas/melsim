

get_proxy_sim_measures <- function() {
  proxy_sim_measures <- summary(proxy::pr_DB)$distance
  proxy_sim_measures[which(!proxy_sim_measures)] %>% names()
}

get_proxy_sim <- function(melody1_obj, melody2_obj, method = get_proxy_sim_measures() ) {

  method <- match.arg(method)

  pitch_sim <- proxy::simil(melody1_obj$data$pitch, melody2_obj$data$pitch, method = method)
  duration_sim <- proxy::simil(melody1_obj$data$duration, melody2_obj$data$duration, method = method)

  mean(c(pitch_sim, duration_sim), na.rm = TRUE)

}

# Try a proxy package method

# melsim('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv', similarity_algorithm = 'Jaccard')

# melsim('data-raw/nokia_829607.csv', 'data-raw/postfinance_2022.csv', similarity_algorithm = 'Jaccard')
