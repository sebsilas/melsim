sim_types <- c("set_based", "sequence_based", "geometric")

sim_transformations <- c("pitch",
                         "pc",
                         "int",
                         "parsons",
                         "ioi_class",
                         "fuzzy_int",
                         "duration_class",
                         "implicit_harmonies",
                         "ngrams")
sim_measures <- c("edit_distance", "ukkon", "sum_common", "count_distinct", "cosine",
                  "Jaccard", "Kulczynski1",  "Kulczynski2", "Mountford",
                  "Fager", "Russel", "Hamman", "Faith",
                  "Tanimoto", "Dice", "Phi", "Stiles", "Michael",
                  "Mozley", "Yule", "Yule2", "Ochiai", "Simpson",
                  "Braun-Blanquet")

sim_measure_factory <- R6::R6Class("SimilarityMeasure",
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
                                   if(!(sim_measure %in% sim_measures)){
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
                                as.character = function(){
                                  sprintf("%s (%s)", self$short, name, self$name)
                                },
                                print = function(){
                                  cat("Similarity measure: \n")
                                  cat("  Name: ", sprintf("%s (%s)", self$name, self$full_name), "\n", sep = "")
                                  cat("  Type: ", self$type, "\n", sep = "")
                                  cat("  Similarity measure: ", self$sim_measure, "\n", sep = "")
                                  cat("  Invariances: \n", sprintf("    Transposition: %s, Tempo: %s",
                                                                   self$transposition_invariant,
                                                                   self$tempo_invariant), "\n", sep = "")
                                  cat("  Cache:", self$cache, "\n")
                                  cat("  Tranformation:  ", "\n    ", self$transformation,"\n", sep = "")
                                  cat("    Parameters:  ", "\n")
                                  cat(sprintf("      %s: %s", names(self$parameters), self$parameters),
                                      "\n", sep = "\n")
                                  invisible(self)
                                }),
                               #end public
                               active = list())

ngram_ukkon <- sim_measure_factory$new(full_name = "ngram-int-3-ukkon",
                                       name = "ngrukkon",
                                       type = "set_based",
                                       transformation = "ngrams",
                                       parameters = list(transformation = "int", ngram_length = 3),
                                       sim_measure = "ukkon",
                                       transposition_invariant = T,
                                       tempo_invariant = T)

ngram_jaccard <- sim_measure_factory$new(full_name = "ngram-int-3-jaccard",
                                       name = "ngrjacc",
                                       type = "set_based",
                                       transformation = "ngrams",
                                       parameters = list(transformation = "int", ngram_length = 3),
                                       sim_measure = "Jaccard",
                                       transposition_invariant = T,
                                       tempo_invariant = T)

harmcore <- sim_measure_factory$new(full_name = "implicit-harmonies-edit-distance",
                                    name = "harmcore",
                                    type = "sequence_based",
                                    transformation = "implicit_harmonies",
                                    parameters = list(),
                                    sim_measure = "edit_distance",
                                    transposition_invariant = T,
                                    tempo_invariant = T)

rhytfuz <- sim_measure_factory$new(full_name = "ioi-class-edit-distance",
                                   name ="rhytfuz",
                                   type = "sequence_based",
                                   transformation = "ioi_class",
                                   parameters = list(),
                                   sim_measure = "edit_distance",
                                   transposition_invariant = T,
                                   tempo_invariant = T)

similarity_measures <- list("ngram_ukkon" = ngram_ukkon,
                            "ngram_jaccard" = ngram_jaccard,
                            "harmcore" = harmcore,
                            "rhythfuz" = rhytfuz)
usethis::use_data(similarity_measures, overwrite = T)
