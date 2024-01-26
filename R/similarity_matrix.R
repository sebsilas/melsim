#'@export
sim_mat_factory <- R6::R6Class(
  "SimilarityMatrix",
  private = list(
    sim_df = tibble(),
    meta = list(name = ""),
    dim1 = 0,
    dim2 = 0,
    symmetric = F,
    type = "invalid",
    diagonal = F,
    melodies = list()
  ),
  #end private
  public = list(
    initialize = function(similarity_df, name = ""){
      #browser()
      private$sim_df <- similarity_df
      private$meta$name <- name
      if(!self$validate(similarity_df)){
        logging::logwarn("Similarity matrix invalid or in wrong format")
      }
    },
    check_homogenity = function(similarity_df){
      sim_df <- similarity_df %>% arrange(algorithm, melody1, melody2)
      if(length(intersect(names(sim_df), c("melody1", "melody2", "sim", "algorithm"))) != 4){
        logging::logwarn("Wrong column names")
        return(FALSE)
      }
      if(sim_df %>% count(algorithm) %>% count(nn = n) %>% nrow() != 1){
        logging::logwarn("Similarity no homogenuous in regard to different algorithms")
        return(FALSE)
      }
      singles <- sim_df %>% group_split(algorithm) %>% lapply(function(x) x %>% select(melody1, melody2))
      if(length(singles) > 1){
        for(i in 1:(length(singles) - 1)){
          if(!identical(singles[[i]], singles[[i + 1]])){
            logging::logwarn("Similarity no homogenuous in regard to different algorithms")
            return(FALSE)
          }
        }

      }
      return(TRUE)
    },
    validate = function(similarity_df){
      #browser()
      private$type <- "invalid"
      if(!is.data.frame(similarity_df)){
        logging::logwarn("No data frame")
        return(FALSE)
      }
      if(length(intersect(names(similarity_df), c("melody1", "melody2", "sim", "algorithm"))) != 4){
        logging::logwarn("Wrong column names")
        return(FALSE)
      }
      #browser()
      if(any(na.omit(similarity_df$sim) > 1) || any(na.omit(similarity_df$sim) < 0)){
        logging::logwarn("Similarity values not in range [0-1]")
        return(FALSE)
      }
      if(!self$check_homogenity(similarity_df)){
        return(FALSE)
      }
      similarity_df <- similarity_df %>% filter(algorithm == algorithm[[1]])
      diag_df <- similarity_df %>% filter(melody1 == melody2)
      diag_sim <- diag_df %>% pull(sim)
      if(length(diag_sim) > 0 && any(diag_sim != 1)){
        logging::logwarn("Self-identity not fulfilled")
        return(FALSE)
      }
      m1 <- similarity_df$melody1
      m2 <- similarity_df$melody2
      l_m1 <- length(m1)
      l_m2 <- length(m2)
      u_m1 <- private$melodies[[1]] <- unique(m1)
      u_m2 <- private$melodies[[2]] <- unique(m2)
      l_u1 <- length(u_m1)
      l_u2 <- length(u_m2)
      private$dim1 <- l_u1
      private$dim2 <- l_u2

      if(length(union(m1, m2)) == length(intersect(m1, m2))){
        private$symmetric <- T
        if(nrow(diag_df) == l_u1){
          private$type <- ifelse(l_m1 == l_u1 * l_u1, "full-diag", "half-diag")
          private$diagonal <- T
        } else {
          private$type <- "full-no-diag"
          private$diagonal <- F
        }
      }
      else if(length(union(m1, m2)) == l_u1 + 1 ||
              length(union(m1, m2)) == l_u2 + 1){
        if(l_m1 == l_u1 * (l_u1 - 1)/2){
          private$type <- "half-no-diag"
          private$has_diagonal <- F
        } else if(l_m1 == l_u1 * (l_u1 + 1)/2){
          private$type <- "half-diag"
          private$dim1 <- l_u1 + 1
          private$dim2 <- l_u2 + 1
          private$diagonal <- F
          private$symmetric <- T
        }
        else{
          private$type <- "general"
          privatediagonal <- nrow(diag_df) > l_u1 || nrow(diag_df) > l_u2
          logging::logwarn("Entries in similarity matrix seem to be misssing")
          return(TRUE)
        }
      } else{
        if(nrow(similarity_df) != l_u1 * l_u2){
          logging::logwarn("Missing entries in non-symmetric data frame")
          return(FALSE)
        }
        private$type <- "general"
      }
      return(TRUE)
    },
    melody_names = function(idx = 1){
      idx <- as.numeric(idx)
      stopifnot(length(idx) > 0)
      idx <- idx[[1]]
      if(idx > 2 || idx < 1){
        return(private$melodies %>% unlist() %>% unique())
      }
      private$melodies[[idx]]
    },
    add_pair_index = function(){
      private$sim_df <- private$sim_df %>%
        mutate(sim_id = pair_index(private$sim_df$melody1,
                                   private$sim_df$melody2))
      invisible(self)
    },
    remove_pair_index = function(){
      if(self$has_pair_index){
        private$sim_df$sim_id <- NULL
      }
      invisible(self)
    },
    as_matrix = function(algorithm = NULL){
      if(!self$is_valid){
        return(NULL)
      }
      nr <- private$dim1
      nc <- private$dim2
      sim_df <- private$sim_df
      if(private$type != "general"){
        sim_df <- self$get_symmetric(with_diagonal = T) %>%
          arrange(algorithm, melody1, melody2)
        nr <- nc <- length(unique(sim_df$melody1))
      }
      if(is.null(algorithm)){
        algorithm <- unique(sim_df$algorithm)
      }
      else{
        algorithm <- intersect(unique(sim_df$algorithm), algorithm)
      }
      if(length(algorithm) == 0){
        stop("Unknown algorithms requested")
      }
      ret <- lapply(algorithm, function(algo){
        tmp <- matrix(sim_df[sim_df$algorithm == algo,]$sim,
                      nrow = nr,
                      ncol = nc)
        row.names(tmp) <- unique(sim_df$melody1)
        colnames(tmp) <- unique(sim_df$melody2)
        tmp
      })
      names(ret) <- algorithm
      if(length(algorithm) == 1){
        return(ret[[1]])
      }
      return(ret)
    },
    as_dist = function(algorithm = NULL, method = "cauchy", ...){
      tmp <- self$as_matrix(algorithm)
      #browser()
      trafo <- function(x, a = 1, b = 1){
        b * (1-x)^a
      }
      if(method == "cauchy"){
        trafo <- function(x, a = 1, b = 1){
          1/(a + b * x) - 1/(a + b)
        }
      }
      if(is.list(tmp)) tmp <- lapply(tmp, function(x) as.dist(trafo(x)))
      else if(length(tmp) == 0) tmp <- NULL
      else tmp <- as.dist(trafo(tmp))
      tmp
    },
    as_wide = function(only_similarities = F){
      tmp <- private$sim_df %>% tidyr::pivot_wider(id_cols = c("melody1", "melody2"),
                                                   names_from = algorithm,
                                                   names_prefix = "sim_",
                                                   values_from = sim
      )
      if(only_similarities){
        tmp <- tmp %>% select(starts_with("sim_"))
      }
      tmp
    },

    make_symmetric = function(){

      private$sim_df <- self$get_symmetric()
      if(self$has_pair_index){
        self$add_pair_index()
      }
      private$type <- "full-diag"
      private$diagonal <- T
      private$symmetric < T
      self
    },

    get_symmetric = function(with_diagonal = T){
      if(private$type == "general") {
        stop("Requesting symmetrizing of general sim matrix, you fool.")
        #return(private$sim_df)
      }
      if(private$type == "full-diag" ) {
        return(private$sim_df)
      }
      algorithms <- unique(private$sim_df$algorithm)
      map_dfr(algorithms, function(sim_algo){
        self$symmetrize(sim_algo, with_diagonal, modify = F)
      })
    },
    symmetrize = function(algorithm, with_diagonal = T, modify = F){
      stopifnot(is.scalar.character(algorithm))
      sim_df <- private$sim_df %>%
        filter(algorithm == !!algorithm)
      full_name <- NULL
      if(self$has_full_name){
        full_name <-  private$sim_df$full_name[1]
      }
      if(private$type == "full-no-diag" && with_diagonal) {
        sim_df <-  sim_df %>%
          bind_rows(
            tibble(
              melody1 = private$melodies[[1]],
              melody2 = private$melodies[[1]],
              sim = 1.0,
              algorithm = algorithm))
        if(!is.null(full_name)){
          sim_df$full_name <- full_name
        }
        return(sim_df)
      }
      sim_df <- sim_df %>%
        bind_rows(
          sim_df %>%
            rename(tmp = melody2) %>%
            rename(melody1 = tmp, melody2 = melody1)
        ) %>% distinct()
      if(with_diagonal){
        sim_df <- sim_df %>%
          distinct() %>%
          filter(melody1 != melody2) %>%
          bind_rows(
            tibble(melody1 = private$melodies[[1]],
                   melody2 = private$melodies[[1]],
                   sim = 1.0, algorithm = algorithm))
        if(!is.null(full_name)){
          sim_df$full_name <- full_name
        }
      }
      if(modify){
        private$sim_df <- sim_df
      }
      sim_df
    },
    plot = function(algorithms = NULL){
      #browser()
      sim_df <- private$sim_df
      if(!is.null(algorithms)){
        sim_df <- sim_df %>% filter(algorithms %in% algorithm)
      }
      q <- sim_df %>% ggplot2::ggplot(aes(x = melody1, y = melody2, fill = sim))
      q <- q + ggplot2::geom_tile()
      q <- q + ggplot2::theme_bw()
      q <- q + ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1))
      q <- q + ggplot2::scale_fill_viridis_c(option = "inferno")
      if(length(unique(sim_df$algorithm)) > 1 && length(unique(sim_df$algorithm)) < 10){
        q <- q + ggplot2::facet_wrap(~algorithm)
      }
      q
    },
    linear_sum = function(algorithms,
                          weights = rep(1.0, length(algorithms)),
                          name = "",
                          add = F){
      #browser()
      algorithms <- unique(algorithms)
      stopifnot(length(algorithms) == length(weights),
                is.vector(algorithms), is.vector(weights))
      tmp <- private$sim_df %>% filter(algorithm %in% algorithms)
      if(length(unique(tmp$algorithm)) != length(algorithms)){
        #browser()
        logging::logwarn(sprintf("Found unknown algorithms: %s",
                                 paste(setdiff(algorithms, tmp$algorithm), collapse = ", ")))
        return(NULL)
      }
      tmp <- tmp %>% tidyr::pivot_wider(id_cols = c("melody1", "melody2"),
                                        names_from = algorithm,
                                        names_prefix = "sim_",
                                        values_from = sim)
      lin_sim <- (tmp %>% select(starts_with("sim")) %>% as.matrix()) %*% weights
      if(is.null(name) || !nzchar(name)){
        name <- sprintf("%.2f * %s", weights, algorithms) %>% paste(collapse = " + ")
      }
      ret <- tmp %>% select(melody1, melody2) %>% mutate(sim = lin_sim[,1],
                                                         algorithm = name,
                                                         full_name = name)
      if(add){
        private$sim_df <- private$sim_df %>% bind_rows(ret)
        return(self)
      }
      ret

    },
    print = function(...) {
      cat("Similarity matrix: \n")
      cat("  Type: ", private$type, "\n", sep = "")
      cat("  Dim: ", sprintf("(%d, %s)", private$dim1, private$dim2), "\n", sep = "")
      cat("  Properties: \n", sprintf("    Symmetric: %s\n    Diagonal: %s",
                                      private$symmetric,
                                      private$diagonal), "\n", sep = "")

      cat("  Metadata:  ", "\n",
          sprintf("    %s: %s",
                  names(private$meta),
                  private$meta),
          "\n", sep = "")
      cat("  Algorithms:  ", paste(unique(private$sim_df$algorithm), collapse = ", "),
          "\n", sep = "")
      invisible(self)
    }
  ),
  #end public
  active = list(
    is_valid = function(){
      private$type != "invalid"
    },
    mat_type = function(){
      private$type
    },
    has_diagonal = function(){
      private$diagonal
    },
    has_full_name = function(){
      "full_name" %in% names(private$sim_df)
    },
    has_pair_index = function(){
      "sim_id" %in% names(private$sim_df)
    },
    data = function(similarity_df){
      if(missing(similarity_df)){
        private$sim_df
      } else{
        stopifnot(self$validate_data(similarity_df))
        private$sim_df <- similarity_df
      }
    }
  ))
test_sim_matrix <- function(){
  sim_df_invalid1 <- tibble(mel1 = "moin", mel2 = "hallo", sim = 1)
  sim_df_invalid2 <- tibble(melody1 = "moin", melody2 = "hallo", sim = 100)
  sim_df_invalid3 <- tibble(melody1 = "moin", melody2 = "moin", sim = .5)
  sim_df_invalid4 <- tibble(melody1 = c("moin", "hello"), melody2 = c("moin", "moin"), sim = c(1, .5))
  sim_df_invalid5 <- tibble(melody1 = c("moin1", "hello1"), melody2 = c("moin2", "moin3"), sim = c(1, .5))
  sim_df_valid1 <- tibble(melody1 = c("moin", "hello", "hello"), melody2 = c("moin", "moin", "hello"), sim = c(1, .5, 1))
  sim_df_valid2 <- tibble(melody1 = c("moin", "hello"), melody2 = c("hello", "moin"), sim = c( .5, .5))
  sim_df_valid3 <- tibble(melody1 = c("moin", "hello", "hello", "moin"), melody2 = c("moin", "moin", "hello", "hello"), sim = c(1, .5, 1, .5))
  sim_df_valid4 <- tibble(melody1 = c("moin1", "hello1", "tach1", "moin1", "hello1", "tach1"), melody2 = c("moin2", "moin2","moin2", "hello2", "hello2", "hello2"), sim = c(.75, .5, .3, .2, .5, .4))
  #sim_mat_factory$new(sim_df_invalid1)
  #sim_mat_factory$new(sim_df_invalid2)
  #sim_mat_factory$new(sim_df_invalid3)
  #sim_mat_factory$new(sim_df_invalid4)
  #sim_mat_factory$new(sim_df_invalid5)
  #sim_mat_factory$new(sim_df_valid1)
  sim_mat_factory$new(sim_df_valid1)
}
