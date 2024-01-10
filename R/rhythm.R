

classify_duration <- Vectorize(
  function(dur_vec, ref_duration = .5){
    rel_dur <- dur_vec/ref_duration
    rhythm_class <- rep(NA, length(rel_dur))
    rhythm_class[rel_dur > 0 & !is.na(rel_dur)] <- -2
    rhythm_class[rel_dur > 0.45] <- -1
    rhythm_class[rel_dur > 0.9] <- 0
    rhythm_class[rel_dur > 1.8] <- 1
    rhythm_class[rel_dur > 3.3] <- 2
    rhythm_class
  })
