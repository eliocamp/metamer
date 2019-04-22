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
#' @param change A character vector with the names of the columns that need to be
#' changed.
#' @param signif The number of significant digits of `preserve` that need to be
#' preserved.
#' @param N Number of iterations.
#' @param trim Max number of metamers to return.
#' @param perturbation Numeric with the magnitude of the random perturbations.
#' @param annealing Logical indicating whether to perform annealing.
#' @param name Character for naming the metamers.
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
#' @seealso [delayed_with()] for a convenient way of making functions suitable for
#' `preserve`, [mean_dist_to()] for a convenient way of minimizing the distance
#' to a known target in `minimize`.
#'
#' @return
#' A `metamer_list` object (a list of data.frames).
#'
#' @references
#' Matejka, J., & Fitzmaurice, G. (2017). Same Stats, Different Graphs. Proceedings of the 2017 CHI Conference on Human Factors in Computing Systems  - CHI ’17, 1290–1294. https://doi.org/10.1145/3025453.3025912
#'
#' @examples
#' data(cars)
#' # Metamers of `cars` with the same mean speed and dist, and correlation
#' # between the two.
#' means_and_cor <- delayed_with(mean_speed = mean(speed),
#'                               mean_dist = mean(dist),
#'                               cor = cor(speed, dist))
#' set.seed(42)  # for reproducibility.
#' metamers <- metamerize(cars,
#'                        preserve = means_and_cor,
#'                        signif = 3,
#'                        N = 1000)
#' print(metamers)
#'
#' last <- metamers[[length(metamers)]]
#'
#' # Confirm that the statistics are the same
#' cbind(original = means_and_cor(cars),
#'       metamer = means_and_cor(last))
#'
#' # Visualize
#' plot(metamers[[length(metamers)]])
#' points(cars, col = "red")
#'
#' @export
#' @importFrom methods hasArg
#' @importFrom stats rnorm runif
metamerize <- function(data,
                       preserve,
                       minimize = NULL,
                       change = colnames(data),
                       signif = 2,
                       N = 100,
                       trim = N,
                       annealing = TRUE,
                       perturbation = 0.08,
                       name = NULL,
                       verbose = interactive()) {
  thiscall <- match.call()
  if (inherits(data, "metamer_list")) {
    preserve     <- .get_attr(data, "preserve", thiscall)
    minimize     <- .get_attr(data, "minimize", thiscall)
    change       <- .get_attr(data, "change", thiscall)
    signif       <- .get_attr(data, "signif", thiscall)
    annealing    <- .get_attr(data, "annealing", thiscall)
    perturbation <- .get_attr(data, "perturbation", thiscall)
    name         <- .get_attr(data, "name", thiscall)[1]
    old_metamers <- data
    data         <- as.data.frame(data[[length(data)]])
  } else {
    old_metamers <- list()
  }

  new_metamers <- metamerize.data.frame(data = data,
                                        preserve = preserve,
                                        minimize = minimize,
                                        change = change,
                                        signif = signif,
                                        N = N,
                                        trim = trim,
                                        annealing = annealing,
                                        perturbation = perturbation,
                                        name = name,
                                        verbose = verbose)
  return(append_metamer(old_metamers, new_metamers))
}


metamerize.data.frame <- function(data,
                                  preserve,
                                  minimize,
                                  change = colnames(data),
                                  signif = 2,
                                  N = 100,
                                  trim = trim,
                                  annealing = TRUE,
                                  perturbation = 0.08,
                                  name = NULL,
                                  verbose = interactive()) {

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


  pb_format <- " :m metamers [:bar] ~ eta: :eta"
  if (!is.null(minimize)) {
    minimize <- match.fun(minimize)
    history[m] <- minimize(data)
    minimize_org <- history[m]
    pb_format <- ":m metamers [:bar] ratio: :d ~ eta: :eta"
  }

  call.args <- list(preserve = preserve,
                    change = change,
                    signif = 2,
                    N = N,
                    minimize = minimize,
                    annealing = annealing)


  new_data <- data

  ncols <- length(change)
  nrows <- nrow(data)
  npoints <- ncols*nrows

  M_temp <- 0.4
  m_temp <- 0.01

  random_pass <- FALSE

  p_bar <- progress::progress_bar$new(total = N, format = pb_format,
                                      clear = FALSE)
  bar_every <- 500

  perturb_ok <- length(perturbation) == 1 || length(perturbation) == ncols
  if (!perturb_ok) {
    stop("perturbation must be of length 1 or ncol(data)")
  }

  for (i in seq_len(N)) {
    if (verbose & (i %% bar_every == 0)) {
      if (!is.null(minimize)) {
        bar_list <- list(m = m, d = signif(history[m]/minimize_org, 2))
      } else {
        bar_list <- list(m = m)
      }
      p_bar$update(i/N, tokens = bar_list)
    }
    temp <- M_temp + ((i-1)/(N-1))^2*(m_temp - M_temp)

    new_change <- matrix(rnorm(npoints, 0, perturbation),
                     nrow = nrows, ncol = ncols, byrow = TRUE)
    new_data[, c(change)] <- metamers[[m]][, c(change)] + new_change

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

  metamers <- new_metamer_list(metamers[seq_len(m)],
                               history[seq_len(m)],
                               preserve,
                               minimize,
                               change,
                               signif,
                               org_exact,
                               annealing,
                               perturbation,
                               name = rep(name, length(m)))
  return(trim(metamers, trim))
}


