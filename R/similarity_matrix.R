sim_mat_factory <- R6::R6Class("SimilarityMatrix",
                               private = list(
                                 sim_df = tibble(),
                                 meta = list(name = ""),
                                 dim1 = 0,
                                 dim2 = 0,
                                 symmetric = F,
                                 type = "invalid",
                                 diag = F,
                                 melodies = list()
                               ),
                               #end private
                               public = list(
                                 initialize = function(similarity_df, name = ""){
                                   #browser()
                                   private$sim_df <- similarity_df
                                   private$meta$name <- name
                                   if(!self$validate(similarity_df)){
                                     warning("Similarity matrix invalid/wrong format")
                                   }
                                 },
                                 validate = function(similarity_df){
                                   #browser()
                                   private$type <- "invalid"
                                   if(!is.data.frame(similarity_df)){
                                     warning("No data frame")
                                     return(FALSE)
                                   }
                                   if(length(intersect(names(similarity_df), c("melody1", "melody2", "sim"))) != 3){
                                     warning("Wrong column names")
                                     return(FALSE)
                                   }
                                   if(any(similarity_df$sim > 1) || any(similarity_df$sim < 0)){
                                     return(FALSE)
                                   }
                                   diag_df <- similarity_df %>% filter(melody1 == melody2)
                                   diag_sim <- diag_df %>% pull(sim)
                                   if(length(diag_sim) > 0 && any(diag_sim != 1)){
                                     warning("Self-identity not fulfilled")
                                     return(FALSE)
                                   }
                                   #browser()
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
                                       private$diag <- T
                                     } else {
                                       private$type <- "full-no-diag"
                                       private$diag <- F
                                     }
                                   }
                                   else if(length(union(m1, m2)) == l_u1 ||
                                           length(union(m1, m2)) == l_u2){
                                     if(l_m1 == l_u1 * (l_u1 - 1)/2){
                                       private$type <- "half-no-diag"
                                       private$diag <- F
                                     } else if(l_m1 == l_u1 * (l_u1 + 1)/2){
                                       private$type <- "half-diag"
                                       private$diag <- T
                                     }
                                     else{
                                       private$type <- "irregular"
                                       private$diag <- nrow(diag_df) > l_u1 || nrow(diag_df) > l_u2
                                       warning("Entries in similarity matrix misssing")
                                       return(FALSE)
                                     }
                                   } else{
                                      if(nrow(similarity_df) != l_u1 * l_u2){
                                        warning("Missing entries in non-symmetric data frame")
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
                                 as.matrix = function(){
                                   browser()
                                   if(!self$is_valid){
                                     return(NULL)
                                   }
                                   nr <- private$dim1
                                   nc <- private$dim2
                                   sim_df <- private$sim_df
                                   if(private$type != "general"){
                                     sim_df <- self$get_symmetric(with_diagonal = T) %>%
                                       arrange(melody1, melody2)
                                     nr <- nc <- length(unique(sim_df$melody1))
                                   }
                                   tmp <- matrix(sim_df$sim, nrow = nr, ncol = nc)
                                   row.names(tmp) <- unique(sim_df$melody1)
                                   colnames(tmp) <- unique(sim_df$melody2)
                                   return(tmp)
                                 },

                                 get_symmetric = function(with_diagonal = T){
                                    if(private$type == "general") {
                                      warning("Resquesting symmetrizing of general sim matrix, you fool.")
                                      return(private$sim_df)
                                    }
                                   if(private$type == "full-diag" ) {
                                     return(private$sim_df)
                                   }
                                   if(private$type == "full-no-diag" && with_diagonal) {
                                     sim_df <- private$sim_df %>%
                                       bind_rows(
                                         tibble(melody1 = private$melodies[[1]],
                                              melody2 = private$melodies[[1]],
                                              sim = 1.0))
                                     return(sim_df)
                                   }
                                   browser()
                                   sim_df <- private$sim_df %>% bind_rows(
                                     private$sim_df %>% set_names(c("melody2", "melody1", "sim"))
                                   ) %>% distinct()
                                   if(with_diagonal){
                                     sim_df <- sim_df %>% distinct() %>% filter(melody1 != melody2) %>%
                                       bind_rows(
                                       tibble(melody1 = private$melodies[[1]],
                                              melody2 = private$melodies[[1]],
                                              sim = 1.0))
                                   }
                                   sim_df
                                 },
                                 print = function(...) {
                                   cat("Similarity matrix: \n")
                                   cat("  Type: ", private$type, "\n", sep = "")
                                   cat("  Dim: ", sprintf("(%d, %s)", private$dim1, private$dim2), "\n", sep = "")
                                   cat("  Properties: ", sprintf("Symmetric: %s, Diagonal: %s",
                                                                 private$symmetric,
                                                                 private$diagonal), "\n", sep = "")

                                   cat("  Metadata :  ", paste(names(private$meta), collapse = ", "), "\n", sep = "")
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
