#' Create metamers
#'
#' Produces very dissimilar datasets with the same statistical properties.
#'
#' @param data A `data.frame` with the starting data or a `metamer_list` object returned
#' by a previous call to the function.
#' @param preserve A function whose result must be kept exactly the same.
#' Must take the data as argument and return a numeric vector.
#' @param minimize An optional function to minimize in the process. Must take
#' the data as argument and return a single numeric.
#' @param cols A character vector with the names of the columns that need to be
#' changed.
#' @param signif The number of significan digits of `preserve` that need to be
#' preserved.
#' @param N Number of iterations.
#' @param annealing Logical indicating whether to perform annealing.
#' @param verbose Logical indicating whether to show a progress bar.
#'
#' @details
#' It follows Matejka & Fitzmaurice (2017) method of constructing metamers.
#' Beginning from a starting dataset, it iteratively adds a small perturbation,
#' checks if `preserve` returns the same value (up to `signif` significant digits)
#' and if `minimize` has been lowered, and accepts the solution for the next
#' round. If `annealing` is `TRUE`, it also accepts solutions with bigger
#' `minimize` with an ever decreasing probability to help the algorithm avoid
#' local minimums.
#'
#' If `data` is a `metamer_list`, the function will start the algorithm from the
#' last metamer of the list. Furthermore, if `preserve` and/or `minimize`
#' are missing, the previous functions will be carried over from the previous call.
#'
#' @return
#' A `metamer_list` object wich is a list of data.frames
#'
#' @references
#' Matejka, J., & Fitzmaurice, G. (2017). Same Stats, Different Graphs. Proceedings of the 2017 CHI Conference on Human Factors in Computing Systems  - CHI ’17, 1290–1294. https://doi.org/10.1145/3025453.3025912
#'
#' @export
#' @importFrom progress progress_bar
metamerize <- function(data,
                       preserve,
                       minimize,
                       signif = 2,
                       N = 100,
                       cols = colnames(data),
                       annealing = TRUE,
                       verbose = interactive(), ...) {
  # on.exit(return(.metamer(metamers, history, m, call.args, org_exact)))

  if (inherits(data, "metamer_list")) {
    metamers <- data
    m <- length(metamers)
    data <- data[[m]]

    if (!hasArg(minimize)) {
      minimize <- attr(metamers, "call.args")$minimize
    }

    if (!is.null(minimize)) {
      history <- attr(metamers, "history")
    }

    if (!hasArg(preserve)) {
      preserve <- attr(metamers, "call.args")$preserve
      org_exact <- attr(metamers, "org_exact")
    }
  } else {
    metamers <- vector(mode = "list", length = N)
    m <- 1
    data <- as.data.frame(data)
    metamers[[m]] <- data

    if (!hasArg(minimize)) {
      minimize <- NULL
    }

    history <- vector(mode = "numeric", length = N)

    preserve <- match.fun(preserve)
    org_exact <- preserve(data)
  }

  pb_format <- " :m metamers [:bar] ~ eta: :eta"
  if (!is.null(minimize)) {
    minimize <- match.fun(minimize)
    history[m] <- minimize(data)
    minimize_org <- history[m]
    pb_format <- ":m metamers [:bar] ratio: :d ~ eta: :eta"
  }

  call.args <- list(preserve = preserve,
                    cols = cols,
                    signif = 2,
                    N = N,
                    minimize = minimize,
                    annealing = annealing)


  new_data <- data

  ncols <- length(cols)
  nrows <- nrow(data)
  npoints <- ncols*nrows

  M_temp <- 0.4
  m_temp <- 0.01

  random_pass <- FALSE

  p_bar <- progress::progress_bar$new(total = N, format = pb_format)
  bar_tick <- 0  # Don't update the progress bar every iteration

  for (i in seq_len(N)) {
    if (verbose & bar_tick == 0) {
      if (!is.null(minimize)) {
        bar_list <- list(m = m, d = signif(history[m]/minimize_org, 2))
      } else {
        bar_list <- list(m = m)
      }

      p_bar$update(i/N, tokens = bar_list)
      bar_tick <- 500
    }
    bar_tick <- bar_tick - 1
    temp <- M_temp + ((i-1)/(N-1))^2*(m_temp - M_temp)

    new_data[, c(cols)] <- metamers[[m]][, c(cols)] + matrix(rnorm(npoints, 0, 0.08),
                                                             nrow = nrows, ncol = ncols)
    new_exact <- preserve(new_data)

    if (!all(signif(new_exact, signif) - signif(org_exact, signif) == 0)) {
      next
    }
    keep <- TRUE
    if (!is.null(minimize)) {
      new_minimize <- minimize(new_data)
      if (annealing) random_pass <- temp > runif(1)

      keep <- (new_minimize <= history[m] || random_pass)
    }

    if (keep) {
      m <- m + 1
      metamers[[m]] <- new_data
      if (!is.null(minimize)) {
        history[m] <- new_minimize
      }
      data <- new_data
    }
  }
  p_bar$terminate()
  return(.metamer(metamers, history, m, call.args, org_exact))
}


.metamer <- function(metamers, history, m, call.args, org_exact) {
  metamers <- metamers[seq_len(m)]
  history <- history[seq_len(m)]
  class(metamers) <- class(metamers) <- c("metamer_list", "list")
  attr(metamers, "history") <- history
  attr(metamers, "call.args") <- call.args
  attr(metamers, "org_exact") <- org_exact
  return(metamers)
}

