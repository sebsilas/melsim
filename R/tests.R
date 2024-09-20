



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


test_mel_sim <- function() {

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
