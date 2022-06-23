#' Stop conditions
#'
#' @param n integer number of tries or metamers.
#' @param r Ratio of minimize value to shoot for. If `0.5`,
#' the stop condition is that the iteration will stop if the value
#' to minimize gets to one-half of the starting value.
#'
#' @export
#' @rdname stop_conditions
n_tries <- function(n) {
  force(n)
  function() {
    get("n_tries", envir = parent.frame()) >= n

  }
}

#' @export
#' @rdname stop_conditions
n_metamers <- function(n) {
  force(n)
  function() {
    get("n_metamers", envir = parent.frame()) >= n
  }
}

#' @export
#' @rdname stop_conditions
minimize_ratio <- function(r) {
  force(r)
  function() {
    get("minimize_ratio", parent.frame()) <= r
  }
}
