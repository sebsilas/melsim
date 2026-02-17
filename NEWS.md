# melsim 0.10.0

# melsim 0.9.3

- Add support for beta regression.

Usage:

```r
recall_sim_measure_beta <- melsim::sim_measure_factory$new(full_name = "recall_similarity_beta",
                                                      name = "recall_similarity_beta",
                                                      transformation = "none",
                                                      parameters = list(keep_singles = TRUE,
                                                                        linear_combination = list(apply_inv_logit_transform = TRUE)
                                                                        ),
                                                      sim_measure = ~ 0.694 * emd + 1.42 * diffed + 1.35 * dtw - 1.92 * const,
                                                      transposition_invariant = TRUE,
                                                      tempo_invariant = TRUE)
                                                      
```

# melsim 0.9.2

# melsim 0.9.1

# melsim 0.9.0



