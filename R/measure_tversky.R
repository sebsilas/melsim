tversky_sim <- function(query_ngrams,
                        target_ngrams,
                        alpha = 1,
                        beta = 1,
                        ngram_db = melsim::int_ngrams_berkowitz) {

  intersect_ngrams <- intersect(query_ngrams, target_ngrams)
  only_query_ngrams <- setdiff(query_ngrams, intersect_ngrams)
  only_target_ngrams <- setdiff(target_ngrams, intersect_ngrams)
  if(is.character(ngram_db)){
    ngram_db <- eval(parse(text = ngram_db), envir = globalenv())
  }
  salience_intersect_ngrams <- get_salience(intersect_ngrams, ngram_db)
  salience_only_query_ngrams <- get_salience(only_query_ngrams, ngram_db)
  salience_only_target_ngrams <- get_salience(only_target_ngrams, ngram_db)
  if(!is.numeric(alpha)){
    alpha <- sum(prop.table(table(query_ngrams))[intersect_ngrams])
  }
  if(!is.numeric(beta)){
    beta <- sum(prop.table(table(target_ngrams))[intersect_ngrams])
  }
  salience_intersect_ngrams / (salience_intersect_ngrams + alpha *  salience_only_target_ngrams + beta *  salience_only_query_ngrams)
}


get_salience <- function(ngrams, ngram_db = melsim::int_ngrams_berkowitz) {
  #idfs <- get_idf_of_ngrams(ngrams$value)
  idfs <- ngram_db %>%
    filter(value %in% ngrams) %>%
    select(ngram = value, idf)
  return(nrow(idfs))
}

