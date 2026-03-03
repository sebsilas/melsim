
read_mel2word_dictionary <- function(path,
                                     conda_env = "mel2word-env",
                                     tidy = FALSE) {

  if (!file.exists(path)) {
    stop("Dictionary file not found: ", path)
  }

  reticulate::use_condaenv(conda_env, required = TRUE)

  mel2word <- reticulate::import("mel2word", delay_load = TRUE)

  py_dict <- mel2word$load_dictionary(path)
  dict_r  <- reticulate::py_to_r(py_dict)

  if (!tidy) {
    return(dict_r)
  }

  tokens <- names(dict_r)
  freq   <- as.integer(unlist(dict_r, use.names = FALSE))

  tibble::tibble(
    token  = tokens,
    freq   = freq,
    length = stringr::str_count(tokens, "_") + 1
  ) |>
    dplyr::arrange(dplyr::desc(freq))
}


train_m2w_w2v <- function(token_list,
                          vector_size = 512,
                          window = 10,
                          min_count = 1,
                          epochs = 300) {

  py$data <- lapply(token_list, function(tokens) {
    list(M2W_all = tokens)
  })

  model <- mel2word$create_word2vec_model_for_M2W(
    py$data,
    feat = "M2W_all",
    vector_size = as.integer(vector_size),
    window = as.integer(window),
    min_count = as.integer(min_count),
    epochs = as.integer(epochs)
  )

  model
}


get_melody_embedding <- function(model, tokens) {

  vecs <- lapply(tokens, function(tok) {
    tryCatch(
      py_to_r(model$wv$`__getitem__`(tok)),
      error = function(e) NULL
    )
  })

  vecs <- vecs[!sapply(vecs, is.null)]

  if (length(vecs) == 0) {
    return(rep(0, model$vector_size))
  }

  colMeans(do.call(rbind, vecs))
}

cosine_sim <- function(x, y) {
  sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
}

