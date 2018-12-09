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


