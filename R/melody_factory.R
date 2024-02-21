#'@export
melody_factory <- R6::R6Class("Melody",

    private = list(
      .mel_data = tibble(onset = numeric(),
                         pitch = numeric()),
      .mel_meta = list(name = "melody"),
      .mel_cache = list(),
      .mel_features = list(),
      .version = packageVersion("melsim")
    ),

    public = list(
      initialize = function(mel_data = tibble(onset = numeric(),
                                              pitch = numeric()),
                            mel_meta = NULL,
                            fname = "",
                            override = F,
                            ...) {
        mel_meta <- safe_append(mel_meta, list(...))
        if(nzchar(fname)){
          tmp <- self$read(fname)
          mel_data <- tmp$mel_data
          mel_meta <- tmp$mel_meta %>% safe_append(mel_meta)
        }
        if(!self$validate_mel_data(mel_data)){
          logging::logerror(sprintf("Invalid melody data (fname = '%s')", fname))
          stop()
        }
        stopifnot(is.list(mel_meta))
        private$.mel_data <- mel_data
        private$.mel_meta <- mel_meta
        self$add_tranforms(transforms = c("int", "fuzzy_int", "parsons", "pc", "ioi", "ioi_class", "int_X_ioi_class"),
                           override = override)
      },

      validate_mel_data = function(mel_data){
        return(is.data.frame(mel_data)  && "onset" %in% names(mel_data) && "pitch" %in% names(mel_data))
      },

      print = function(...) {
        cat("Melody: \n")
        cat("  Number of notes: ", nrow(private$.mel_data), "\n", sep = "")
        cat("  Metadata:  ", "\n",
            sprintf("    %s: %s\n",
                    names(private$.mel_meta),
                    private$.mel_meta),
            sep = "")
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
        invisible(self)
      },

      add_tranforms = function(transforms = c("int", "fuzzy_int", "parsons", "pc", "ioi", "ioi_class", "int_X_ioi_class"),
                               override = TRUE){
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
        if("int_X_ioi_class" %in% transforms){
          if(override || self$has_not("int_X_ioi_class")){
            int_X_ioi_class <- sprintf("(%s|%s)",
                                       c(diff(private$.mel_data$pitch), NA),
                                       classify_duration(c(diff(private$.mel_data$onset), NA)))
            int_X_ioi_class[str_detect(int_X_ioi_class, "NA")] <- NA
            private$.mel_data$int_X_ioi_class <- int_X_ioi_class
          }
        }
        invisible(self)
      },

      add_ngrams = function(columns, N, override = T){
        private$.mel_data <- add_ngrams(private$.mel_data, columns = columns, N = N, override = override)
        invisible(self)
      },

      has = function(col_name){
        !is.null(col_name) && col_name %in% names(private$.mel_data)
      },

      has_not = function(col_name){
        !(col_name %in% names(private$.mel_data))
      },

      remove_columns = function(col_names){
        to_keep <- union(c("onset", "pitch"), setdiff(names(private$.mel_data), col_names))
        #print(to_keep)
        private$.mel_data <- private$.mel_data[, to_keep]
        invisible(self)
      },

      transpose = function(col, value){
        if(is.scalar.character(col) & self$has(col)){
          tmp <- private$.mel_data
          tmp[[col]] <- tmp[[col]] + methods::as(value, class(tmp[[col]]))
          private$.mel_data  <- tmp
        }
        invisible(self)
      },

      split_by = function(segmentation, seg_prefix = "%s"){
        if(self$has_not(segmentation)){
          return(invisible(self))
        }
        segments <- unique(private$.mel_data[[segmentation]])
        if(!("name" %in% names(private$.mel_meta))){
          tmp_name <- "MELODY"
          if("file_name" %in% names(private$.mel_meta)){
            tmp_name <- tools::file_path_sans_ext(basename(private$.mel_meta$file_name))
          }
          self$add_meta("name", tmp_name)
        }
        name_template <- sprintf("%%s_%s", seg_prefix)
        map(segments, function(seg_id){
          melody_factory$new(mel_data = private$.mel_data %>%
                               filter(!!sym(segmentation) == seg_id),
                             mel_meta = private$.mel_meta,
                             override = T,
                             segment = seg_id)$add_meta("name",
                                                        sprintf(name_template,
                                                                private$.mel_meta$name,
                                                                as.integer(seg_id)))
        })
      },

      read = function(fname){
        ext <- file_ext(fname)
        if(ext %in% c("csv", "mcsv")){
          return(self$read_mcsv(fname))
        }
        if(ext %in% c("krn", "kern")){
          stop("Not implemented")
        }
        if(ext %in% c("mid", "midi")){
          stop("Not implemented")
        }
        if(ext %in% c("xml",  "musicxml")){
          #stop("Not implemented")
          list(mel_data = read_musicxml(fname),
               mel_meta = list(file_name = fname,
                               name = tools::file_path_sans_ext(basename(fname))))
        }
      },

      read_mcsv = function(fname){
        mel_data <- read.csv(fname,
                             header = TRUE,
                             sep = ";",
                             stringsAsFactors = FALSE) %>%
          as_tibble()
        mel_meta <- list(file_name =  fname, name = tools::file_path_sans_ext(basename(fname)))
        list(mel_data = mel_data, mel_meta = mel_meta)
      },

      get_implicit_harmonies = function(segmentation = "bar", only_winner = TRUE, cache = TRUE){
        ih_id <- sprintf("%s_%s",
                         ifelse(is.null(segmentation), "global", segmentation),
                         ifelse(only_winner, "best", "full"))
        if(cache && "implicit_harmonies" %in% names(private$.mel_cache)){
          ih <- private$.mel_cache$implicit_harmonies[[ih_id]]
          if(!is.null(ih)){
            return(ih)
          }
        }
        if(self$has(segmentation)){
          segmentation <- private$.mel_data[[segmentation]]
        }
        else{
          #logging::logwarn(sprintf("Requested segmentation '%s' not found, using const", segmentation))
          segmentation <- rep(1, nrow(private$.mel_data))
        }
        if(self$has("ioi_class")){
          weights <- 2^(private$.mel_data$ioi_class - 1)
        }
        else{
          weights <- NULL
        }
        ih <- get_implicit_harmonies(private$.mel_data$pitch,
                                     segmentation,
                                     only_winner = only_winner,
                                     weights = weights,
                                     fast_algorithm = T)
        if(cache){
          if(is.null(private$.mel_cache$implicit_harmonies)){
            private$.mel_cache$implicit_harmonies <- list()
          }
          private$.mel_cache$implicit_harmonies[[ih_id]] <- ih
        }
        return(ih)
      },

      edit_sim = function(melody, transform = "int", sim_measure = edit_sim_utf8, optimizer = NULL){
        if(transform == "implicit_harmonies"){
          ih1 <- self$get_implicit_harmonies() %>% pull(key)
          ih2 <- melody$get_implicit_harmonies() %>% pull(key)
          common_keys <- levels(factor(union(ih1, ih2)))
          ih1 <- factor(ih1, levels = common_keys) %>%
            as.integer()
          ih2 <- factor(ih2, levels = common_keys) %>%
            as.integer()
          return(sim_measure(ih1, ih2))
        }
        if(self$has_not(transform)){
          self$add_transforms(transform)
        }
        if(melody$has_not(transform)){
          melody$add_transforms(transform)
        }
        v1 <- private$.mel_data[[transform]] %>% na.omit() %>% unclass()
        v2 <- melody$data[[transform]] %>% na.omit() %>% unclass()
        if(length(v1) == 0 || length(v2) == 0){
          return(NA)
        }
        if(is.numeric(v1) && is.numeric(v2)){
          #browser()
          #offset <- min(c(v1, v2)) - 1
          #v1 <- v1 - offset
          #v2 <- v2 - offset
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
        if(!is.null(optimizer)){
          return(optim_transposer(v1, v2, sim_measure = sim_measure, strategy = "all"))
        }
        sim_measure(v1, v2)
      },

      ngram_similarity = function(melody,
                                  N = 3,
                                  transform = "int",
                                  method = "ukkon",
                                  modify = TRUE,
                                  parameters = NULL){
        stopifnot(N > 0, methods::is(melody, "Melody"), transform %in% names(private$.mel_data))
        #browser()
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
        ngrams1 <- na.omit(mel1$data[[ngr]]) %>% as.vector()
        ngrams2 <- na.omit(mel2$data[[ngr]]) %>%  as.vector()
        if(is.function(method)){
          method(ngrams1, ngrams2)
        }
        else if(method == "count_distinct"){
          count_distinct(ngrams1, ngrams2)
        }
        else if(method == "sum_common"){
          sum_common(ngrams1, ngrams2)
        }
        else if(method == "ukkon"){
          ukkon(ngrams1, ngrams2)
        }
        else if(method == "dist_sim"){
          dist_sim(ngrams1, ngrams2)
        }
        else if(method == "Tversky"){
          tversky_sim(ngrams1,
                      ngrams2,
                      alpha = parameters$alpha,
                      beta = parameters$beta,
                      ngram_db = parameters$ngram_db
          )
        }
        else if(proxy::pr_DB$entry_exists(method)){
          proxy_simil(ngrams1, ngrams2, method)
        }
        else{
          loggin::logerror("Similarity function %s not defined", method)
          retunr(NA)
        }
      },

      similarity = function(melody, sim_measures){
        if(!is.list(sim_measures)){
          sim_measures <- list(sim_measures)
        }
        stopifnot(methods::is(melody, "Melody"),
                  all(sapply(sim_measures, validate_sim_measure)))

        purrr::imap_dfr(sim_measures, function(sm, i){
          if(is.scalar.character(sm)){
            sm_str <- sm
            sm <- similarity_measures[[sm_str]]
            if(is.null(sm)){
              logging::logerror(sprintf("Unknown similarity measure: %s", sm_str))
              return(NULL)
            }
          }
          if(sm$type == "sequence_based"){
            sim <- self$edit_sim(melody,
                                 sm$transformation,
                                 sim_measure = eval(parse(text = sm$sim_measure)),
                                 optimizer = sm$parameters$optimizer)
            return(tibble(algorithm = sm$name, full_name = sm$full_name, sim = sim))
          }
          else if(sm$type == "set_based"){
            if(sm$transformation == "ngrams"){
              sim <- self$ngram_similarity(melody,
                                           N = sm$parameters$ngram_length,
                                           transform = sm$parameters$transform,
                                           method = sm$sim_measure,
                                           modify = sm$cache,
                                           parameters = sm$parameters
                                           )
              return(tibble(algorithm = sm$name, full_name = sm$full_name, sim = sim))
            }
            else{
              logging::logerror(sprintf("Transformation %s not implemented yet for set_based", sm$transformation))
              return(NULL)
            }
          }
          else if (sm$type == "linear_combination"){
            lin_comb <- parse_linear_combination(sm$sim_measure)
            all_sm <- melsim::similarity_measures
            missing_sm <- setdiff(lin_comb$terms, names(all_sm))
            if(length(missing_sm) > 0){
              logging::logerror(sprintf("Unrecognized similarity measures in linear combination: %s",
                                        paste(missing_sm, collapse = ", ")))
              return(NULL)
            }
            sms <- all_sm[lin_comb$terms]
            single_sims <- self$similarity(melody, sms) %>%
              left_join(lin_comb %>%
                          rename(algorithm = terms),
                        by = "algorithm")
            keep <- safe_get(sm$parameters, "keep_singles")
            combi_sim <-
              tibble(algorithm = sm$name,
                     full_name = sm$full_name,
                     sim = squeeze(sum(single_sims$sim * single_sims$weights), 0, 1))

            if(keep){
              ret <- single_sims %>%
                select(-weights) %>%
                bind_rows(combi_sim)
            }
            else{
              ret <- combi_sim            }
            #logging::logerror(sprintf("Linear combination failed to compute"))
            return(ret)
          } else if (sm$type == "special"){
            if(sm$sim_measure == "const"){
              return(tibble(algorithm = sm$name, full_name = sm$full_name, sim = 1.0))
            }
            if(sm$sim_measure == "sim_NCD"){
              #browser()
              stopifnot(methods::is(melody, "Melody"),
                        sm$transformation %in% names(self$data),
                        sm$transformation %in% names(melody$data))

              sim <- sim_NCD(paste(na.omit(self$data[[sm$transformation]]), collapse = ""),
                             paste(na.omit(melody$data[[sm$transformation]]), collapse = ""))
              return(tibble(algorithm = sm$name, full_name = sm$full_name, sim = sim))
            }
            if(sm$sim_measure == "sim_emd"){
              #browser()
              stopifnot(methods::is(melody, "Melody"))
              if(!is.null(sm$parameters$optimizer)){
                sim <- optim_transposer_emd(private$.mel_data,
                                            melody$data,
                                            beta = sm$parameters$beta, strategy = "all")
                return(tibble(algorithm = sm$name, full_name = sm$full_name, sim = sim))
              }

              sim <- sim_emd(mel1 = private$.mel_data,
                             mel2 = melody$data,
                             beta = sm$parameters$beta)
              return(tibble(algorithm = sm$name, full_name = sm$full_name, sim = sim))
            }
            if(sm$sim_measure == "sim_dtw"){
              stopifnot(methods::is(melody, "Melody"))
              sim <- sim_dtw(private$.mel_data,
                             melody$data,
                             beta = sm$parameters$beta)
              return(tibble(algorithm = sm$name, full_name = sm$full_name, sim = sim))
            }

            logging::logwarn(sprintf("Special measure: %s not implemented.", sm$sim_measure))
            return(NULL)
          }
          else{
            logging::logwarn(sprintf("Type: %s not implemented.", sm$type))
            return(NULL)
          }

        })
      },

      add_difficulty_features = function(segmentation = NULL,
                                         override = TRUE){
        if(!is.null(segmentation) && self$has(segmentation)){
          tmp <- private$.mel_data %>%
            group_by(!!sym(segmentation)) %>%
            summarise(across(int, interval_difficulty), .groups = "drop") %>%
            unnest(int)
        }
        else{
          segmentation <- "global"
          tmp <- private$.mel_data %>%
            summarise(across(int, interval_difficulty), .groups = "drop") %>%
            unnest(int)
        }
        self$.add_features(tmp, segmentation, override, prefix = "DIFF")
        invisible((self))
      },

      add_basic_features = function(columns = c("pitch", "int", "fuzzy_int"),
                              func_list = list(mean = mean,
                                               abs_mean = abs_mean,
                                               sd = sd,
                                               abs_sd = abs_sd,
                                               entropy  = entropy_wrapper),
                              segmentation = NULL,
                              override = TRUE) {

        common_cols <- intersect(names(private$.mel_data), columns)
        if(!is.null(segmentation) && self$has(segmentation)){
          tmp <- private$.mel_data %>%
            group_by(!!sym(segmentation)) %>%
            summarise(across(all_of(common_cols), func_list, na.rm = TRUE), .groups = "drop")
        }
        else{
          segmentation <- "global"
          tmp <- private$.mel_data %>%
            summarise(across(all_of(common_cols),
                             func_list, na.rm = TRUE), .groups = "drop")
        }
        self$.add_features(tmp, segmentation, override, prefix = "BASE")
        invisible((self))
      },

      add_tonal_features = function(segmentation = NULL, override = T){
        if(is.null(segmentation)){
          segmentation <- "global"
        }
        ih_id <- sprintf("%s_full", segmentation)
        ih <- NULL
        if("implicit_harmonies" %in% names(private$.mel_cache)){
          ih <- private$.mel_cache$implicit_harmonies[[ih_id]]
        }
        if(is.null(ih)){
          ih <- self$get_implicit_harmonies(segmentation = segmentation,
                                            cache = T,
                                            only_winner = F)
        }
        tf <- get_tonal_features(ih) %>% rename(!!segmentation := segment)
        self$.add_features(tf, segmentation, override, prefix ="TON")

      },
      .add_features  = function(features, segmentation = NULL, override = T, prefix = ""){
        if(is.null(segmentation)){
          segmentation <- "global"
        }
        if(nzchar(prefix)){
          features <- features %>% set_names(sprintf("%s.%s", prefix, names(.)))
        }
        if(is.null(private$.mel_features[[segmentation]]) ||
           nrow(private$.mel_features[[segmentation]]) == 0){
          private$.mel_features[[segmentation]] <- features
        }
        else {
          if(override){
            feature_cols <- names(features)
            del_cols <- intersect(names(private$.mel_features[[segmentation]]), feature_cols)
            if(length(del_cols) > 0){
              private$.mel_features[[segmentation]] <-
                private$.mel_features[[segmentation]] %>%
                select(-all_of(del_cols))
            }
          }
          else{
            feature_cols <- setdiff(names(features), names(private$.mel_features[[segmentation]]))
          }
          if(length(feature_cols) > 0) {
            private$.mel_features[[segmentation]] <- bind_cols(private$.mel_features[[segmentation]],
                                               features[,feature_cols])
          }
        }
        invisible((self))
      },

      plot = function(){
        if(self$length == 0 ){
          stop("No melody data to plot")
        }

        q <- private$.mel_data %>% ggplot2::ggplot(aes(x = onset, y = pitch))
        q <- q + ggplot2::geom_segment(aes(xend = onset + ioi, yend = pitch))
        q <- q + ggplot2::theme_bw()
        q
      }
    ),


    # End public
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

      cache = function(value){
        if(missing(value)){
          private$.mel_cache
        } else{
          if(is.list(value)){
            private$.mel_cache <- value
          }
        }
      },

      meta = function(value){
        if(missing(value)){
          private$.mel_meta
        } else{
          if(is.list(value)){
            private$.mel_data <- value
          }
        }
      },

      features = function(value){
        if(missing(value)){
          private$.mel_features
        } else{
          if(is.list(value)){
            private$.mel_features <- value
          }
        }
      },

      length =  function(){
        nrow(private$.mel_data)
      },

      pitch = function(){
        private$.mel_data$pitch
      },

      onset = function(){
        private$.mel_data$onset
      },

      version = function(value){
        if(missing(value)){
          private$.version
        } else{
          if(is.scalar.character(value)){
            private$.version <- value
          }
        }
      }
    )
)
