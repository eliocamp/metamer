#' Set `metamer_list` attributes
#'
#' Set attributes of `metamer_list`s that will be used as function arguments in
#' [metamerize()].
#'
#' @param object A `metamer_list` object.
#' @param minimize,preserve Minimize and preserve functions as defined in
#'  [metamerize()].
#'
#'
#' @export
set_minimize <- function(object, minimize) {
  attr(object, "minimize") <- minimize
  object
}

#' @rdname set_minimize
get_minimize <- function(object) {
  attr(object, "minimize")
}

#' @rdname set_minimize
set_preserve <- function(object, preserve) {
  attr(object, "preserve") <- preserve
  object
}

#' @rdname set_minimize
get_preserve <- function(object) {
  attr(object, "preserve")
}
