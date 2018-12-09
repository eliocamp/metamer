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
#' `with()`, but you pass the data after.
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
#' some_stats <- reverse_with(mean_x = mean(x), mean(y), sd(x), coef(lm(x ~ y)))
#' data <- data.frame(x = rnorm(20) , y = rnorm(20))
#' some_stats(data)
#'
#' @export
reverse_with <- function(...) {
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
