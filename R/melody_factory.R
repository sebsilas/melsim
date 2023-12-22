library(R6)
library(tidyverse)

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

abs_mean <- function(x, ...) mean(abs(x), ...)
abs_sd <- function(x, ...) sd(abs(x), ...)
safe_entropy <- function(x, ...) suppressWarnings(entropy::entropy(na.omit(x)))

add_ngrams <- function(mel_data, columns, N, override = F) {
  ngram_cols <-
    map_dfc(columns, function(col){
      if(!(col %in% names(mel_data))){
        return(NULL)
      }
      ngrams <- get_all_ngrams(mel_data[[col]],
                               N,
                               keep_length = T) %>%
        pivot_wider(id_cols = start,
                    names_from = N,
                    names_prefix = sprintf("%s_ngram_", col),
                    values_from = value) %>%
        select(-start)
      ngrams
    })
  if(!override){
    new_cols <- setdiff(names(ngram_cols), names(mel_data))
    bind_cols(mel_data, ngram_cols %>% select(new_cols))
  }
  else{
    new_cols <- names(ngram_cols)
    bind_cols(mel_data[, setdiff(names(mel_data), new_cols)], ngram_cols)
  }
}

melody_factory <- R6Class("Melody",
    private = list(
      .mel_data = tibble(onset = numeric(),
                         pitch = numeric()),
      .mel_meta = list(name = "melody"),
      .mel_features = tibble()
    ),

    public = list(
      initialize = function(mel_data = tibble(onset = numeric(),
                                              pitch = numeric()),
                            mel_meta = list(name = "melody"),
                            fname = "",
                            format = "") {
        if(nzchar(fname) && nzchar(format)){
          if(substr(format, 1, 4) == "mcsv"){
            mel_data <- read.csv(fname, header = T, sep = ";", stringsAsFactors = F) %>% as_tibble()
            mel_meta <- list(file_name =  fname)

          }
        }
        stopifnot(self$validate_mel_data(mel_data))
        stopifnot(is.list(mel_meta))
        private$.mel_data <- mel_data %>%
          mutate(across(where(is.integer), as.numeric)) %>%
          mutate(phrase_id = 1)
        private$.mel_meta <- mel_meta
        self$add_tranforms(transforms = c("int", "fuzzy_int", "parsons", "pc", "ioi", "ioi_class"), override = F)
        self$add_meta("title", "My Title")
      },

      validate_mel_data = function(mel_data){
        return(is.data.frame(mel_data)  && "onset" %in% names(mel_data) && "pitch" %in% names(mel_data))
      },

      print = function(...) {
        cat("Melody: \n")
        cat("  Num events: ", nrow(private$.mel_data), "\n", sep = "")
        cat("  Metadata fields:  ", paste(names(private$.mel_meta), collapse = ", "), "\n", sep = "")
        invisible(self)
      },

      add_meta = function(field, value){
        if(missing(value)){
          if(is.character(field))
            private$.mel_meta[field]
        } else{
          if(is.scalar.character(field)){
            private$.mel_meta[[field]] <- value
          }
        }
      },

      add_tranforms = function(transforms = c("int", "fuzzy_int", "parsons", "pc", "ioi", "ioi_class"), override = T){
        if(length(transforms) == 1  && is.function(transforms)){
          tmp <- transforms(private$.mel_data)
          if(intersect(names(private$.mel_data), names(tmp)) == names(private$.mel_data) &&
             override &&
             nrow(tmp) == nrow(privat$.mel_data)){
            private$.mel_data <- tmp
          }
        }
        if("int" %in% transforms){
          if(override || self$has_not("int")){
            private$.mel_data$int <- c(diff(private$.mel_data$pitch), NA)
          }
        }
        if("fuzzy_int" %in% transforms){
          if(override || self$has_not("fuzzy_int")){
            private$.mel_data$fuzzy_int <- fuzzyint_class(c(diff(private$.mel_data$pitch), NA))
          }
        }
        if("parsons" %in% transforms){
          if(override || self$has_not("parsons")){
            private$.mel_data$parsons <- sign(c(diff(private$.mel_data$pitch), NA))
          }
        }
        if("pc" %in% transforms){
          if(override || self$has_not("pc")){
            private$.mel_data$pc <- private$.mel_data$pitch %% 12
          }
        }
        if("ioi" %in% transforms){
          if(override || self$has_not("ioi")){
            private$.mel_data$ioi <- c(diff(private$.mel_data$onset), NA)
          }
        }
        if("ioi_class" %in% transforms){
          if(override || self$has_not("ioi_class")){
            private$.mel_data$ioi_class <- classify_duration(c(diff(private$.mel_data$onset), NA))
          }
        }
        invisible(self)
      },
      add_ngrams = function(columns, N, override = T){
        private$.mel_data <- add_ngrams(private$.mel_data, columns = columns, N = N, override = override)
        invisible(self)
      },
      has = function(col_name){
        col_name %in% names(private$.mel_data)
      },
      has_not = function(col_name){
        !(col_name %in% names(private$.mel_data))
      },
      remove_columns = function(col_names){
        to_keep <- union(c("onset", "pitch"), setdiff(names(private$.mel_data), col_names))
        print(to_keep)
        private$.mel_data <- private$.mel_data[, to_keep]
        invisible(self)

      },
      transpose = function(col, value){
        if(is.scalar.character(col) & self$has(col)){
          tmp <- private$.mel_data
          tmp[[col]] <- tmp[[col]] + as(value, class(tmp[[col]]))
          private$.mel_data  <- tmp
        }
        invisible(self)
      },
      read_mcsv = function(fname){
        private$.mel_data <- read.csv(fname, header = T, sep = ";", stringsAsFactors = F) %>% as_tibble()
        self$add_tranforms(transforms = c("int", "fuzzy_int", "parsons", "pc", "ioi", "ioi_class"), override = F)
        self$add_meta("file_name", fname)
        invisible(self)
      },
      get_implicit_harmonies = function(segmentation = NULL, only_winner = T, cache = T){
        ih_id <- sprintf("%s_%s",
                         ifelse(is.null(segmentation), "none", segmentation),
                         ifelse(only_winner, "best", "full"))
        if(cache && "implicit_harmonies" %in% names(private$.mel_meta)){
          ih <- private$.mel_meta$implicit_harmonies[[ih_id]]
          if(!is.null(ih)){
            return(ih)
          }
        }
        if(self$has(segmentation)){
          segmentation <- private$.mel_data[[segmentation]]
        }
        ih <- get_implicit_harmonies(private$.mel_data$pitch, segmentation, only_winner = only_winner)
        if(cache){
          if(is.null(private$.mel_meta$implicit_harmonies)){
            private$.mel_meta$implicit_harmonies <- list()
          }
          private$.mel_meta$implicit_harmonies[[ih_id]] <- ih
        }
        return(ih)
      },
      edit_sim = function(melody, transform = "ioi_class"){
        if(transform == "harm"){
          ih1 <- self$get_implicit_harmonies() %>% pull(key)
          ih2 <- melody$get_implicit_harmonies() %>% pull(key)
          common_keys <- levels(factor(union(ih1, ih2)))
          ih1 <- factor(ih1, levels = common_keys) %>%
            as.integer()
          ih2 <- factor(ih2, levels = common_keys) %>%
            as.integer()
          return(edit_sim(intToUtf8(ih1), intToUtf8(ih2)))
        }
        if(self$has_not(transform)){
          self$add_transforms(transform)
        }
        if(melody$has_not(transform)){
          melody$add_transforms(transform)
        }
        v1 <- private$.mel_data[[transform]] %>% na.omit() %>% unclass()
        v2 <- melody$data[[transform]] %>% na.omit() %>% unclass()
        if(is.numeric(v1) && is.numeric(v2)){
          v1 <- v1 - min(v1) + 1
          v2 <- v2 - min(v2) + 1
        }
        else if(is.character(v1) && is.character(v2)){
          common_elts <- levels(factor(union(v1, v2)))
          v1 <- factor(v1, levels = common_elts) %>%
            as.integer()
          v2 <- factor(v2, levels = common_elts) %>%
            as.integer()

        } else{
          stop("Undefined edit similarity")
        }

        edit_sim(intToUtf8(v1), intToUtf8(v2))
      },
      ngram_similarity = function(melody, N = 3, transform = "int", method = "ukkon", modify = T){
        stopifnot(N > 1, is(melody, "Melody"), transform %in% names(private$.mel_data))
        browser()
        ngr <- sprintf("%s_ngram_%d", transform, N)
        if(self$has_not(ngr)){
          if(!modify){
            mel1 <- self$clone()
          }
          else{
            mel1 <- self
          }
          mel1$add_ngrams(columns = transform, N = N)
        }
        else{
          mel1 <- self
        }
        if(melody$has_not(ngr)){
          if(!modify){
            mel2 <- melody$clone()
          }
          else{
            mel2 <- melody
          }
          mel2$add_ngrams(columns = transform, N = N)
        }
        else{
          mel2 <- melody
        }
        if(is.function(method)){
          method(mel1$data[[ngr]], mel2$data[[ngr]])
        }
        else if(method == "count_distinct"){
          count_distinct(mel1$data[[ngr]], mel2$data[[ngr]])
        }
        else if(method == "sum_common"){
          sum_common(mel1$data[[ngr]], mel2$data[[ngr]])
        }
        else if(method == "ukkon"){
          ukkon_similarity(mel1$data[[ngr]], mel2$data[[ngr]])
        }
        else{
          stop("Similarity function not defined")
        }

      },
      add_features = function(columns = c("pitch", "int", "fuzzy_int"),
                              func_list = list(mean = mean, abs_mean = abs_mean, sd = sd, abs_sd = abs_sd),
                              segmentation = NULL,
                              override = T
                              ){
        common_cols <- intersect(names(private$.mel_data), columns)
        if(!is.null(segmentation) && self$has(segmentation)){
          tmp <- private$.mel_data %>%
            group_by(!!sym(segmentation)) %>%
            summarise(across(all_of(common_cols), func_list, na.rm = T), .groups = "drop")
        }
        else{
          tmp <- private$.mel_data %>% summarise(across(all_of(common_cols), func_list, na.rm = T))
        }
        if(nrow(private$.mel_features) == 0){
          private$.mel_features <- tmp
        }
        else {
          if(override){
            feature_cols <- names(tmp)
          }
          else{
            feature_cols <- setdiff(names(tmp), names(private$.mel_features))
          }
          if(length(feature_cols) > 0) {
            private$.mel_features <- bind_cols(private$.mel_features, tmp[,feature_cols])
          }
        }
        invisible((self))
      }
    ),
    #end public
    #####################
    active = list(
      data = function(value){
        if(missing(value)){
          private$.mel_data
        } else{
          stopifnot(self$validate_data(value))
          private$.mel_data <- value
        }
      },
      meta = function(value){
        if(missing(value)){
          private$.mel_meta
        } else{
          if(is.list(meta)){
            private$.mel_data <- value
          }
        }
      },
      features = function(value){
        if(missing(value)){
          private$.mel_features
        } else{
          if(is.list(meta)){
            private$.mel_features <- value
          }
        }
      },
      length =  function(){
        nrow(private$.mel_data)
      }

    )
)

