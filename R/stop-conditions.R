#' @export
n_tries <- function(n) {
  force(n)
  function() {
    get("n_tries", envir = parent.frame()) >= n

  }
}

#' @export
n_metamers <- function(n) {
  force(n)
  function() {
    get("n_metamers", envir = parent.frame()) >= n
  }
}

#' @export
minimize_ratio <- function(x) {
  force(x)
  function() {
    get("minimize_ratio", parent.frame()) <= x
  }
}
