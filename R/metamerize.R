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
#' Can be of length 1 or `length(change)`.
#' @param annealing Logical indicating whether to perform annealing.
#' @param K speed/quality tradeoff parameter.
#' @param start_probability initial probability of rejecting bad solutions.
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
#' The annealing scheme is adapted from de Vicente et al. (2003).
#'
#' If `data` is a `metamer_list`, the function will start the algorithm from the
#' last metamer of the list. Furthermore, if `preserve` and/or `minimize`
#' are missing, the previous functions will be carried over from the previous call.
#'
#' `minimize` can be also a *vector* of functions. In that case, the process minimizes
#' the product of the functions applied to the data.
#'
#' @seealso [delayed_with()] for a convenient way of making functions suitable for
#' `preserve`, [mean_dist_to()] for a convenient way of minimizing the distance
#' to a known target in `minimize`, [mean_self_proximity()] for maximizing the
#' "self distance" to prevent data clumping.
#'
#'
#'
#' @return
#' A `metamer_list` object (a list of data.frames).
#'
#' @references
#' Matejka, J., & Fitzmaurice, G. (2017). Same Stats, Different Graphs. Proceedings of the 2017 CHI Conference on Human Factors in Computing Systems  - CHI ’17, 1290–1294. https://doi.org/10.1145/3025453.3025912
#' de Vicente, Juan, Juan Lanchares, and Román Hermida. (2003). ‘Placement by Thermodynamic Simulated Annealing’. Physics Letters A 317(5): 415–23.
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
                       K = 0.02,
                       start_probability = 0.5,
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
    K            <- .get_attr(data, "K", thiscall)
    start_probability <- .get_attr(data, "start_probability", thiscall)
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
                                        verbose = verbose,
                                        K = K,
                                        start_probability = start_probability)
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
                                  K = 0.002,
                                  start_probability = 0.4,
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
    if (length(minimize) > 1) {
      min_funs <- minimize
      minimize <- function(data) {
        Reduce("*", lapply(seq_along(minimize), function(i) match.fun(min_funs[[i]])(data)))
      }
    } else {
      minimize <- match.fun(minimize)
    }

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

  perturb_ok <- length(perturbation) == 1 || length(perturbation) == ncols
  if (!perturb_ok) {
    stop("perturbation must be of length 1 or length(change)")
  }
  M_temp <- 0
  if (annealing & !is.null(minimize)) {
    # To estimate initial temperature, create random samples, compute
    # minimize and take the average absolute change. Initial
    # temperature then is that value /log(0.4) so that on average
    # the algo initially accepts bad solutiosn with 40% probability.
    warmup <- 50
    tries <- replicate(warmup, {
      new_change <- matrix(rnorm(npoints, 0, perturbation),
                           nrow = nrows, ncol = ncols, byrow = TRUE)

      old <- as.matrix(metamers[[m]][change])
      new <- old + new_change

      new_data[change] <- new
      minimize(new_data)
    })

    M_temp <- -mean(abs(tries - history[m]))/log(start_probability)
    temp <- M_temp
    m_temp <- 0

  }

  random_pass <- FALSE

  p_bar <- progress::progress_bar$new(total = N, format = pb_format,
                                      clear = FALSE)
  bar_every <- 500

  worse <- 0
  delta_cost <- 0
  cost_change <- 0
  for (i in seq_len(N)) {
    if (verbose & (i %% bar_every == 0)) {
      if (!is.null(minimize)) {
        bar_list <- list(m = m, d = signif(history[m]/minimize_org, 2))
      } else {
        bar_list <- list(m = m)
      }
      p_bar$update(i/N, tokens = bar_list)
    }


    new_change <- matrix(rnorm(npoints, 0, perturbation),
                         nrow = nrows, ncol = ncols, byrow = TRUE)

    old <- as.matrix(metamers[[m]][change])
    new <- old + new_change

    new_data[change] <- new

    new_exact <- preserve(new_data)

    if (!all((signif(new_exact, signif) - signif(org_exact, signif)) == 0)) {
      next
    }
    keep <- TRUE
    if (!is.null(minimize)) {
      new_minimize <- minimize(new_data)
      random_pass <- FALSE

      if (annealing) {
        # browser()
        # temp <- M_temp + ((i-1)/(N-1))^2*(m_temp - M_temp)
        # temp <- max(temp*.8, m_temp)
        # temp <- M_temp - (i - 1) * (M_temp - m_temp)/(N - 1)

        cost_change <- (new_minimize - history[m])
        if (cost_change > 0) {
          worse <- worse + cost_change/temp
        }

        prob <- ifelse(cost_change > 0, exp(-(cost_change/temp)), 1)

        random_pass <- runif(1) <= prob
      }

      keep <- (new_minimize <= history[m] || random_pass)
    }

    if (keep) {
      delta_cost <- delta_cost + cost_change
      m <- m + 1
      metamers[[m]] <- new_data
      if (!is.null(minimize)) {
        history[m] <- new_minimize
      }
      data <- new_data
    }

    if (annealing) {
      if (worse == 0 || delta_cost >= 0) {
        temp <- M_temp
      } else {
        temp <- -K*delta_cost/worse
      }

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
                               name = rep(name, length(m)),
                               K= K,
                               start_probability = start_probability)
  return(trim(metamers, trim))
}


