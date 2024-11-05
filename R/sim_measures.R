
#'@export
sim_measure_factory <- R6::R6Class(
  "SimilarityMeasure",
  private = list(
  ),
  # End private
  public = list(
    name = "",
    full_name = "",
    transformation = "",
    parameters = list(),
    sim_measure = "",
    type = "",
    transposition_invariant = FALSE,
    tempo_invariant = FALSE,
    cache = FALSE,
    initialize = function(name = "",
                          full_name = "",
                          transformation = "",
                          parameters = list(),
                          sim_measure = "",
                          type = get_sim_type(sim_measure),
                          transposition_invariant = F,#if(transformation == "ngrams") melsim:::is_tempo_invariant(parameters$transform) else melsim:::is_tempo_invariant(transformation),
                          tempo_invariant = F,#if(transformation == "ngrams") melsim:::is_tempo_invariant(parameters$transform, parameters$optimizer) else melsim:::is_tempo_invariant(transformation),
                          cache = TRUE
    ) {
      #print(name)
      stopifnot(purrr::is_scalar_character(name),
                purrr::is_scalar_character(full_name),
                purrr::is_scalar_character(type),
                purrr::is_scalar_character(transformation),
                is.list(parameters),
                purrr::is_scalar_character(sim_measure) || purrr::is_formula(sim_measure),
                purrr::is_scalar_logical(transposition_invariant),
                purrr::is_scalar_logical(tempo_invariant))
      if(!transformation %in% sim_transformations) {
        stop(sprintf("Unrecognized transformation: %s", transformation))
      }
      if(transformation == "ngrams") {
        if(!is.list(parameters)) {
          stop("Ngram transformation needs parameter lists")
        }
        if(length(intersect(names(parameters),
                            c("transformation", "ngram_length"))) != 2) {
          stop("Ngram needs parameter 'transformation' and 'ngram_length'")
        }
        if(!(parameters$transformation %in% sim_transformations)) {
          stop(sprintf("Unrecognized transformation for ngrams: %s",
                       parameters$transformation))
        }

      }

      # Check if sim measure recognised
      if(purrr::is_formula(sim_measure)) {
        terms <- parse_linear_combination(sim_measure)$terms
        purrr::walk(terms, is_official_sim_measure)
      } else {
        is_sim_measure_recognised(sim_measure)
      }

      self$name <- name
      self$full_name <- full_name
      self$type <- type
      self$transformation <- transformation
      self$parameters <- parameters
      self$sim_measure <- sim_measure
      self$transposition_invariant <- transposition_invariant
      self$tempo_invariant  <- tempo_invariant
      self$cache <- cache
    },
    as_character = function() {
      sprintf("%s (%s)", self$short, name, self$name)
    },
    as_list = function() {
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
    write_yaml = function(fname, fileEncoding ="UTF-8") {
      l <- self$as_list()
      name <- l$name
      self$as_list()
      l <- list(l)
      names(l) <- name
      yaml::write_yaml(l, fname)
    },
    print = function() {
      logging::loginfo("Similarity measure info:")
      logging::loginfo("Name: %s (%s)", self$name, self$full_name)
      logging::loginfo("Type: %s", self$type)
      logging::loginfo("Similarity measure: %s", self$sim_measure)
      logging::loginfo("Invariances:")
      logging::loginfo("Transposition: %s", self$transposition_invariant)
      logging::loginfo("Cache: %s", self$cache)
      logging::loginfo("Tranformation: %s", self$transformation)
      logging::loginfo("Parameters:")
      logging::loginfo(paste(sprintf("  %s: %s", names(self$parameters), self$parameters), collapse = ", "))
      invisible(self)
    }),
  # End public
  active = list())





#'@export
sim_types <- c("set_based",
               "distribution_based",
               "sequence_based",
               "vector_based",
               "linear_combination",
               "special")

#'@export
sim_transformations <- c("pitch",
                         "pc",
                         "int",
                         "parsons",
                         #"ioi",
                         #"phrase_segmentation",
                         "ioi_class",
                         "fuzzy_int",
                         "duration_class",
                         "implicit_harmonies",
                         "int_X_ioi_class",
                         "ngrams",
                         "none")

proxy_pkg_measures <-  proxy::pr_DB$get_entry_names()

#'@export
set_based_measures <- c(
  # from melsim:
  "count_distinct",
  "Tversky",
  # from proxy:
  "Jaccard",
  #"Kulczynski1", unbounded
  "Kulczynski2",
  #"Mountford",  not normalized
  "Russel", #"Fager",
  "simple matching",
  #"Hamman",
  "Faith", "Tanimoto", "Dice",
  #"Phi", -1 to 1
  #"Stiles", > 1
  #"Michael",-1 to 1
  "Mozley",
  #"Yule",-1 to 1
  #"Yule2",-1 to 1
  "Ochiai",
  "Simpson",
  "Braun-Blanquet"

  )


#'@export
vector_measures <- c(
  "cosine", "angular", "correlation",
  #"Chi-squared", unbounded
  #"Phi-squared",unbounded
  "Tschuprow",
   "Cramer",
  #"Pearson", not bnormed
  "Gower", "Euclidean",
  #"Mahalanobis", not normalizable
   "Manhattan",
  "supremum", "Minkowski", "Canberra", "Chord",  "Geodesic",
  "Bray",   "Soergel", "Podani", "Whittaker", "eJaccard", "eDice"
  #"Wave", not normed
  )

#'@export
distribution_based_measures <- c(
  #"fJaccard", only for 0 < values <1
  "ukkon",
  "sum_common",
  "distr_sim",
  "Bhjattacharyya",
  "divergence",
  "Hellinger"

  #"Kullback" # from proxy, not symmetric

)

#'@export
sequence_based_measures <- c(
  "edit_sim_utf8", "edit_sim", # from melsim
  "Levenshtein",
  "sim_NCD",#compression distance
  "stringdot_utf8" # based on kernlab::stringdot
  )

#'@export
special_measures <- c(
  "pmi", "const", "sim_emd", "sim_dtw" # from melsim

  )

#'@export
low_level_sim_measures <- c(set_based_measures, vector_measures, sequence_based_measures, distribution_based_measures)


proxy_pkg_types <- c("binary", "metric", "nominal", "other")


# Functions

is_proxy_pkg_measure <- function(sim_measure) {
  proxy::pr_DB$entry_exists(sim_measure)
}

get_sim_type <- Vectorize(function(sim_measure) {
  if(is_formula(sim_measure)) {
    return("linear_combination")
  } else if(sim_measure %in% vector_measures) {
    return("vector_based")
  } else if(sim_measure %in% sequence_based_measures) {
    return("sequence_based")
  } else if(sim_measure %in% distribution_based_measures) {
    return("distribution_based")
  } else if(sim_measure %in% set_based_measures) {
    return("set_based")
  } else if(sim_measure %in% special_measures) {
    return("special")
  } else {
    stop(sprintf("sim_measure: <%s>  not recognised.", as.character(sim_measure)))
  }
})

get_proxy_sim_measure_type <- function(sim_measure_name) {
  if(sim_measure_name == "Gower") {
    return("mixed")
  }
  proxy::pr_DB$get_entry(sim_measure_name)$type
}

is_distance_measure <- Vectorize(function(sim_measure_name) {
  if(is_proxy_pkg_measure(sim_measure_name)) {
    return(proxy::pr_DB$get_entry(sim_measure_name)$distance)
  } else {
    if(sim_measure_name %in% c("pmi", "const")) {
      return(NA)
    } else if(sim_measure_name == "Levenshtein") { # Technically also covered by proxy, but we have a separate implementation
      return(TRUE)
    } else if(sim_measure_name %in% c("sim_emd", "sim_dtw", "edit_sim_utf8", "edit_sim", "sim_NCD", "stringdot_utf8", "count_distinct", "Tversky", "ukkon", "sum_common", "distr_sim")) {
      return(FALSE)
    } else {
      stop(paste0("`sim_measure_name` ", sim_measure_name, " not found."))
    }
  }
})

proxy_pkg_measures_types <- purrr::map_dfr(proxy_pkg_measures, function(measure) {
  tibble::tibble(
    measure = measure,
    type =  get_proxy_sim_measure_type(measure)
  )
})

sim_measure_from_string <- function(sm) {
  if(is.scalar.character(sm)) {
    sm_str <- sm
    sm <- similarity_measures[[sm_str]]
    if(is.null(sm)) {
      logging::logerror(sprintf("Unknown similarity measure: %s", sm_str))
      return(NULL)
    }
  } else {
    return(sm)
  }
}



#' @export
get_sim_measures <- function(){
  names(similarity_measures)
}

#'@export
is_official_sim_measure <- function(names) {
  names %in% get_sim_measures()
}

is_sim_measure <- function(sim_measures) {
  methods::is(sim_measures, "SimilarityMeasure")
}

is_sim_measure_recognised <- function(sim_measure) {
  if(!(sim_measure %in% low_level_sim_measures) &&
     !(validate_sim_measure(sim_measure))) {
    stop(sprintf("Unrecognized similarity measure: %s", sim_measure))
  }

}

