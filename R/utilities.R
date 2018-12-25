#' Mean minimum distance
#'
#' Creates a function to get the mean minimum distance between two sets of points.
#'
#' @param target A `data.frame` with all numeric columns.
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
mean_dist_to <- function(target) {
  target <- as.matrix(target)
  function(data) {
    mean(FNN::get.knnx(target, as.matrix(data), k = 1)$nn.dist)
  }
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
#' Each expression in `...` must return numeric values. They can be named or
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


#' Trim a `metamer_list`
#'
#' When creating metamers, [metamerize()] can produce thousands of very similar
#' metamers. This function is intended to keep only a subset of them for easier
#' and faster handling and plotting.
#'
#' @param object A `metamer_list` object returned by [metamerize()]
#' @param n The number of metamers to keep.
#'
#' @return
#' A `metamer_list` object with `n` equally spaced metamers.
#'
#' @export
trim <- function(object, n = length(object)) {
  N <- length(object)
  new <- object[seq(1, N, length.out = n)]
  mostattributes(new) <- attributes(object)
  new
}


#' Compute moments
#'
#' Returns a function that will return uncentered moments
#'
#' @param orders Numeric with the order of the uncentered moments that will
#' be computed.
#' @param cols Character vector with the name of the columns of the data for which
#' moments will be computed
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
