#' Mean minimum distance
#'
#' Creates a function to get the mean minimum distance between two sets of points.
#'
#' @param target A `data.frame` with all numeric columns.
#' @param squared Logical indicating whether to compute the mean squared
#' distance (if `TRUE`) or the mean distance.
#'
#' @return
#' A function that takes a `data.frame` with the same number of columns as
#' `target` and then returns the mean minimum distance between them.
#'
#' @examples
#' target <- data.frame(x = rnorm(100), y = rnorm(100))
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' distance <- mean_dist_to(target)
#' distance(data)
#'
#' @family helper functions
#' @export
mean_dist_to <- function(target, squared = TRUE) {
  force(target)
  if (squared) {
    d <- 2
  } else {
    d <- 1
  }

  target <- as.matrix(target)
  function(data) {
    mean(FNN::get.knnx(target, as.matrix(data), k = 1)$nn.dist^d)
  }
}

#' Mean distance to an sf object
#'
#' @param target An sf object.
#' @param coords Character vector with the columns of the data object
#' that define de coordinates.
#' @param buffer Buffer around the sf object. Distances smaller
#' than `buffer` are replaced with 0.
#' @param squared Logical indicating whether to compute the mean squared
#' distance (if `TRUE`) or the mean distance.
#'
#'
#' @family helper functions
#' @export
mean_dist_to_sf <- function(target, coords = c("x", "y"), buffer = 0, squared = TRUE) {
  if (!requireNamespace("sf", quietly = TRUE)) {
    stop("'mean_dist_to_sf' needs the sf package installed. Install it with 'install.packages(\"sf\").")
  }

  force(target)
  force(coords)
  if (squared) {
    d <- 2
  } else {
    d <- 1
  }
  function(data) {
    data <- sf::st_as_sf(data, coords = coords)
    ds <- sf::st_distance(data, target)
    ds[ds < buffer] <- 0
    mean(ds^d)
  }
}

#' Inverse of the mean self distance
#'
#' Returns the inverse of the mean minimum distance between different pairs of points.
#' It's intended to be used as a minimizing function to, then, maximize the distance between
#' points.
#'
#' @param data a data.frame
#'
#' @family helper functions
#' @export
mean_self_proximity <- function(data) {
  data <- as.matrix(data)
  1/mean(FNN::get.knnx(data, data, k = 2)$nn.dist[, 2])
}

#' Apply expressions to data.frames
#'
#' Creates a function that evaluates expressions in a future data.frame. Is like
#' `with()`, but the data argument is passed at a later step.
#'
#' @param ... Expressions that will be evaluated.
#'
#' @return
#' A function that takes a `data.frame` and returns the expressions in `...`
#' evaluated in an environment constructed from it.
#'
#' @details
#' Each expression in `...` must return a single numeric value. They can be named or
#' return named vectors.
#'
#' @examples
#' some_stats <- delayed_with(mean_x = mean(x), mean(y), sd(x), coef(lm(x ~ y)))
#' data <- data.frame(x = rnorm(20) , y = rnorm(20))
#' some_stats(data)
#'
#' @family helper functions
#' @export
delayed_with <- function(...) {
  funs <- match.call(expand.dots = FALSE)$`...`
  function(data) {
    unlist(lapply(funs, function(x) eval(x, data)))
  }
}


#' Rounding functions
#'
#' @param digits Number of significant digits.
#' @family helper functions
#' @export
#' @rdname rounding
truncate_to <- function(digits) {
  base <- 10^digits
  function(x) {
    trunc(x*base)/base
  }

}

#' @family helper functions
#' @export
#' @rdname rounding
round_to <- function(digits) {
  force(digits)
  function(x) {
    round(x, digits)
  }

}

trim <- function(object, n = length(object)) {
  N <- length(object)
  if (n == 2) {
    keep <- c(1, N)
  } else if (n == 1) {
    keep  <- N
  } else if (N > n){
    gap <- round((N - 3)/(n - 2))
    keep <- c(1, round(seq(1 + gap, N - gap, length.out = n - 2)), N)
  } else {
    keep <- seq_len(N)
  }

  new <- object[keep]
  mostattributes(new) <- attributes(object)
  attr(new, "name") <- attr(object, "name")[keep]
  new
}


#' Compute moments
#'
#' Returns a function that will return uncentered moments
#'
#' @param orders Numeric with the order of the uncentered moments that will
#' be computed.
#' @param cols Character vector with the name of the columns of the data for which
#' moments will be computed. If `NULL`, will use all columns.
#'
#' @return
#' A function that takes a `data.frame` and return a named numeric vector of the
#' uncentered moments of the columns.
#'
#' @examples
#' data <- data.frame(x = rnorm(100), y = rnorm(100))
#' moments_3 <- moments_n(1:3)
#'
#' moments_3(data)
#'
#' moments_3 <- moments_n(1:3, "x")
#' moments_3(data)
#'
#' @family helper functions
#' @export
moments_n <- function(orders, cols = NULL) {
  force(orders)
  force(cols)
  function(data) {
    if (is.null(cols)) cols <- colnames(data)
    ms <- unlist(lapply(cols, function(col) moments_(data[[col]], orders)))
    stats::setNames(ms, paste0(rep(cols, each = length(orders)), "_", orders))
  }
}

moment_ <- function(x, order) {
  sum(x^order)/length(x)
}

moments_ <- function(x, orders) {
  vapply(orders, function(o) moment_(x, o), 1)
}


#' Increase resolution of data
#'
#' Interpolates between the output of [draw_data()] and increases the point
#' density of each stroke.Useful for avoiding sparse targets that result in
#' clumping of points when metamerizing. It only has an effect on strokes (made
#' by double clicking).
#'
#' @param data A `data.frame` with columns `x`, `y` and `.group`.
#' @param res A numeric indicating the multiplicative resolution (i.e. 2 =
#' double resolution).
#'
#' @return
#' A `data.frame` with the `x` and `y` values of your data and a `.group` column
#' that identifies each stroke.
#'
#' @family  helper functions
#' @export
densify <- function(data, res = 2) {
  data <- by(data, data$.group,
             function(d) {
               N <- nrow(d)
               id <- seq_len(N)
               if (N > 1) {
                 data.frame(x = stats::approx(id, d$x, n = N*res)$y,
                            y = stats::approx(id, d$y, n = N*res)$y,
                            .group = d$.group)
               } else {
                 d
               }
             })
  do.call(rbind, data)
}

.datatable.aware <- TRUE
