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
#' @param round A function to apply to the result of `preserve` to round
#' numbers. See [truncate_to].
#' @param stop_if A stopping criterium. See [n_tries].
#' @param keep Max number of metamers to return.
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
#'                        round = truncate_to(2),
#'                        stop_if = n_tries(1000))
#' print(metamers)
#'
#' last <- tail(metamers)
#'
#' # Confirm that the statistics are the same
#' cbind(original = means_and_cor(cars),
#'       metamer = means_and_cor(last))
#'
#' # Visualize
#' plot(tail(metamers))
#' points(cars, col = "red")
#'
#' @export
#' @importFrom methods hasArg
#' @importFrom stats rnorm runif
metamerise <- function(data,
                       preserve,
                       minimize = NULL,
                       change = colnames(data),
                       round = truncate_to(2),
                       stop_if = n_tries(100),
                       keep = NULL,
                       annealing = TRUE,
                       K = 0.02,
                       start_probability = 0.5,
                       perturbation = 0.08,
                       name = "",
                       verbose = interactive()) {
  if (inherits(data, "data.frame")) {
    data <- metamer_list$new(data, preserve = preserve, round = round)
  }

  data <- data$clone()

  if (hasArg("minimize")) {
    data$set_minimize(minimize)
  }
  if (hasArg("change")) {
    data$set_change(change)
  }
  if (hasArg("K")) {
    data$set_k(K)
  }
  if (hasArg("perturbation")) {
    data$set_perturbation(perturbation)
  }
  if (hasArg("start_probability")) {
    data$set_start_probability(start_probability)
  }
  if (hasArg("annealing")) {
    data$set_annealing(annealing)
  }

  data$metamerise(stop_if = stop_if,
                  name = name,
                  keep = keep,
                  verbose = verbose)
  return(data)
}

#' @export
#' @rdname metamerise
metamerize <- metamerise

#' @export
#' @rdname metamerise
new_metamer <- function(data, preserve, round = truncate_to(2)) {
  metamer_list$new(data, preserve = preserve, round = round)
}
