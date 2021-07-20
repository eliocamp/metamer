metamer_list <- R6::R6Class("metamer_list",
  public = list(
    preserve = NULL,
    signif = NULL,
    pb_format = ":name -> :spin :n_metamers n_metamers",
    minimize_fun = NULL,
    metamers = NULL,
    history = NA,

    annealing = TRUE,
    perturbation = 0.08,
    start_probability = 0.5,
    K = 0.02,
    change = NULL,
    names = "",

    initialize = function(data,
                          preserve = NULL,
                          signif   = 2) {
      self$preserve <-  preserve
      self$signif <-  signif
      self$metamers <- list(data)
      self$set_change(colnames(data))

      return(invisible(self))
    },

    print = function(...) {
      cat("List of ", length(self$metamers), " metamers", sep = "")
      return(invisible(self))
    },

    minimize = function(data) {
      if (is.null(self$minimize_fun)) {
        return(NA)
      }

      self$minimize_fun(data)
    },

    clear_minimize = function() {
      self$set_minimize(minimize = NULL)
      return(invisible(self))
    },

    set_minimize = function(minimize) {
      pb_format <- ":name -> :spin :n_metamers n_metamers"

      if (is.null(minimize)) {
        self$minimize_fun <- NULL
        self$pb_format <- ":name -> :spin :n_metamers n_metamers"
        return(invisible(self))
      }

      if (length(minimize) > 1) {
        min_funs <- minimize
        minimize_fun <- function(data) {
          Reduce("*", lapply(seq_along(minimize), function(i) match.fun(min_funs[[i]])(data)))
        }
      } else {
        minimize_fun <- match.fun(minimize)
      }

      self$minimize_fun <- minimize_fun

      if (tail(is.na(self$history), 1)) {
        self$history <- self$minimize(self$last_metamer())
      }

      self$pb_format <- ":name -> :spin :n_metamers metamers ratio: :minimize_ratio"

      return(invisible(self))
    },

    last_metamer = function() {
      return(tail(self$metamers, 1)[[1]])
    },

    set_annealing = function(annealing) {
      self$annealing <- annealing
      return(invisible(self))
    },

    set_perturbation = function(perturbation) {
      perturbation_ok <- length(perturbation) == 1 || length(perturbation) == ncol(self$last_metamer())

      if (!perturbation_ok) {
        stop("'perturbation' must be length 1 or length ", ncol(self$last_metamer()), ".")
      }

      self$perturbation <- perturbation
      return(invisible(self))
    },

    set_start_probability = function(start_probability) {
      self$start_probability <- start_probability
      return(invisible(self))
    },

    set_K = function(K) {
      self$K <- K
      return(invisible(self))
    },

    set_change = function(change) {
      if (is.null(change)) {
        change <- colnames(self$last_metamer())
      }

      self$change <-  change
      return(invisible(self))
    },

    as.data.table = function() {
      if (!requireNamespace("data.table", quietly = TRUE)) {
        stop("'as.data.table' needs the data.table package installed. Install it with 'install.packages(\"data.table\").")
      }
      id_metamers <- data.table::data.table(.metamer = seq_along(self$metamers),
                                           .name = self$names)
      metamers <- data.table::rbindlist(self$metamers, idcol = ".metamer")
      metamers <- id_metamers[metamers, on = ".metamer"]
      return(metamers)
    },

    as.data.frame = function() {
      id_metamers <- data.frame(.name = self$names,
                                .metamer = seq_along(self$metamers))

      df <- lapply(seq_along(self$metamers), function(x)  {
        df <- self$metamers[[x]]
        df[[".metamer"]] <- x
        df[[".name"]] <- self$names[x]
        df
      })
      df <- do.call(rbind, df)

      return(df)
    },

    metamerise = function(stop_if = n_tries(10),
                          name = "",
                          keep = NULL,
                          verbose = interactive()) {
      upper_bound <- 1e6
      new_metamers <- vector(mode = "list", length = upper_bound)
      new_history <- rep(NA, length = upper_bound)

      data <- self$last_metamer()
      stats <- signif(self$preserve(data), self$signif)
      data <- as.data.frame(data)

      n_tries <- 0

      minimize_first_value <- self$minimize(data)
      minimize_previous_value <- minimize_first_value
      minimize_value <- minimize_first_value

      minimize_ratio <- 1

      will_minimize <- !is.na(minimize_first_value)

      self$names[length(self$names)] <- name

      # Don't do annealing if minimize returns NA
      annealing <- self$annealing & !is.na(minimize_first_value)

      n_metamers <- 0

      ncols <- length(self$change)
      nrows <- nrow(data)
      npoints <- ncols*nrows
      perturbation <- self$perturbation
      change <- self$change

      perturb_data <- function(data) {
        new_change <- matrix(rnorm(npoints, 0, perturbation),
                             nrow = nrows, ncol = ncols,
                             byrow = TRUE)

        old <- as.matrix(data[change])
        new <- old + new_change

        data[change] <- new

        return(data)
      }

      # Warmup for annealing
      if (annealing) {
        delta_cost <- 0
        worse <- 0

        n_warmups <- 50
        tries <- replicate(n_warmups, {
          self$minimize(perturb_data(data))
        })
        starting_temperature <- -mean(abs(tries - minimize_first_value))/log(self$start_probability)
        temperature <- starting_temperature
      }

      keep_going <- TRUE

      p_bar <- progress::progress_bar$new(total = NA,
                                          format = self$pb_format,
                                          clear = TRUE)
      preserve <- self$preserve

      signif_num <- self$signif
      minimize <- self$minimize
      bar_every <- 500

      while (keep_going) {
        # Are we there yet?
        keep_going <- !stop_if()

        if (!keep_going) {
          next
        }
        n_tries <- n_tries + 1

        if (verbose) {
          if (n_tries %% bar_every == 0) {
            bar_list <- list(n_tries = n_tries,
                             name = name,
                             minimize_ratio = signif(minimize_ratio, 2),
                             n_metamers = n_metamers)
            p_bar$tick(0, tokens = bar_list)
          }
        }

        data_try <- perturb_data(data)
        new_stats <- preserve(data_try)
        if (!all((signif(new_stats, signif_num) - stats) == 0)) {
          next
        }

        keep_this <- TRUE

        if (will_minimize) {
          minimize_value <- minimize(data_try)
          cost_change <- minimize_value - minimize_previous_value
          if (!is.na(cost_change) && cost_change <= 0) {
            keep_this <- TRUE
          } else if (annealing) {
            if (cost_change > 0) {
              worse <- worse + cost_change/temperature
            }

            aproval_prob <- ifelse(cost_change > 0, exp(-(cost_change/temperature)), 1)
            keep_this <- runif(1) <= aproval_prob
          }

          if (!keep_this) {
            next
          }

          if (annealing) {
            delta_cost <- delta_cost + cost_change
            if (worse == 0 || delta_cost >= 0) {
              temperature <- starting_temperature
            } else {
              temperature <- -self$K*delta_cost/worse
            }
          }

          minimize_ratio <- minimize_value/minimize_first_value
          minimize_previous_value <- minimize_value
        }

        n_metamers <- n_metamers + 1
        new_metamers[[n_metamers]] <- data_try
        new_history[n_metamers] <- minimize_value
        data <- data_try
      }
      p_bar$terminate()
      if (is.null(keep)) {
        keep <- n_metamers
      }
      self$metamers <- c(self$metamers, trim(new_metamers[seq_len(n_metamers)], keep))
      self$history  <- c(self$history,  trim(new_history[n_metamers], keep))
      self$names    <- c(self$names,    trim(rep(name, n_metamers), keep))


      return(invisible(self))
    },
    metamerize = function(...) {
      self$metamerise(...)
    }
  )

)
