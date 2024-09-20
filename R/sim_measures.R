sim_types <- c("set_based", "sequence_based", "vector_based", "linear_combination", "special")

sim_transformations <- c("pitch",
                         "pc",
                         "int",
                         "parsons",
                         "ioi_class",
                         "fuzzy_int",
                         "duration_class",
                         "implicit_harmonies",
                         "int_X_ioi_class",
                         "ngrams",
                         "none")

sim_measures <- c("edit_sim_utf8", "edit_sim",
                  "ukkon", "sum_common", "count_distinct", "dist_sim",
                  "cosine",
                  "Jaccard", "Kulczynski1",  "Kulczynski2", "Mountford",
                  "Fager", "Russel", "Hamman", "Faith",
                  "Tanimoto", "Dice", "Phi", "Stiles", "Michael",
                  "Mozley", "Yule", "Yule2", "Ochiai", "Simpson",
                  "Braun-Blanquet", "Tversky", "pmi", "const",
                  "sim_NCD", "sim_emd")
#' @export
get_sim_measures <- function(){
  #sapply(melsim::similarity_measures, function(x) x$name) %>% as.character()
  names(similarity_measures)
}

#'@export
is_sim_measure <- function(names){
  names %in% get_sim_measures()
}

#'@export
sim_measure_factory <- R6::R6Class(
  "SimilarityMeasure",
  private = list(
  ),
  #end private
  public = list(
    name = "",
    full_name = "",
    type = "",
    transformation = "",
    parameters = list(),
    sim_measure = "",
    transposition_invariant = FALSE,
    tempo_invariant = FALSE,
    cache = F,
    initialize = function(name = "",
                          full_name = "",
                          type = "",
                          transformation = "",
                          parameters = list(),
                          sim_measure = "",
                          transposition_invariant = FALSE,
                          tempo_invariant = FALSE,
                          cache = T
    ){
      stopifnot(purrr::is_scalar_character(name),
                purrr::is_scalar_character(full_name),
                purrr::is_scalar_character(type),
                purrr::is_scalar_character(transformation),
                is.list(parameters),
                purrr::is_scalar_character(sim_measure),
                purrr::is_scalar_logical(transposition_invariant),
                purrr::is_scalar_logical(tempo_invariant))
      if(!(type %in% sim_types)){
        stop(sprintf("Unrecognized type: %s", type))
      }
      if(!(transformation %in% sim_transformations)){
        stop(sprintf("Unrecognized transformation: %s", transformation))
      }
      if(transformation == "ngrams"){
        if(!is.list(parameters)){
          stop("Ngram transformation needs parameter lists")
        }
        if(length(intersect(names(parameters),
                            c("transformation", "ngram_length"))) != 2){
          stop("Ngram needs parameter 'transformation' and 'ngram_lengths'")
        }
        if(!(parameters$transformation %in% sim_transformations)){
          stop(sprintf("Unrecognized transformation for ngrams: %s",
                       parameters$transformation))
        }

      }
      if(!(sim_measure %in% sim_measures) && !(validate_sim_measure(sim_measure))){
        stop(sprintf("Unrecognized similarity measure: %s", sim_measure))
      }

      self$name <- name
      self$full_name <- full_name
      self$type <- type
      self$transformation <- transformation
      self$parameters <- parameters
      self$sim_measure <- sim_measure
      self$transposition_invariant <- transposition_invariant
      self$tempo_invariant  <-  tempo_invariant
      self$cache <- cache
    },
    as_character = function(){
      sprintf("%s (%s)", self$short, name, self$name)
    },
    as_list = function(){
      x <- self %>% as.list()
      x[c("name",
          "full_name",
          "type",
          "transformation",
          "parameters",
          "sim_measure",
          "transposition_invariant",
          "tempo_invariant",
          "cache"
      )]

    },
    write_yaml = function(fname, fileEncoding ="UTF-8"){
      l <- self$as_list()
      name <- l$name
      self$as_list()
      l <- list(l)
      names(l) <- name
      yaml::write_yaml(l, fname)
    },
    print = function(){
      cat("Similarity measure: \n")
      cat("  Name: ", sprintf("%s (%s)", self$name, self$full_name), "\n", sep = "")
      cat("  Type: ", self$type, "\n", sep = "")
      cat("  Similarity measure: ", self$sim_measure, "\n", sep = "")
      cat("  Invariances: \n", sprintf("    Transposition: %s\n    Tempo: %s",
                                       self$transposition_invariant,
                                       self$tempo_invariant), "\n", sep = "")
      cat("  Cache:", self$cache, "\n")
      cat("  Tranformation:  ", "\n    ", self$transformation,"\n", sep = "")
      cat("    Parameters:  ", "\n")
      cat(sprintf("      %s: %s\n", names(self$parameters), self$parameters),
          sep = "")
      invisible(self)
    }),
  #end public
  active = list())

