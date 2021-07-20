#' Set metamer parameters
#'
#' @param metamer_list A `metamer_list` object.
#' @inheritParams metamerize
#'
#' @export
#' @rdname set_params
clear_minimize <- function(metamer_list) {
  metamer_list$clone()$clear_minimize()
}
#' @export
#' @rdname set_params
set_minimize <- function(metamer_list, minimize) {
  metamer_list$clone()$set_minimize(minimize)
}
#' @export
#' @rdname set_params
get_last_metamer <- function(metamer_list) {
  metamer_list$last_metamer()
}
#' @export
#' @rdname set_params
set_annealing <- function(metamer_list, annealing) {
  metamer_list$clone()$set_annealing(annealing)
}
#' @export
#' @rdname set_params
set_perturbation <- function(metamer_list, perturbation) {
  metamer_list$clone()$set_perturbation(perturbation)
}
#' @export
#' @rdname set_params
set_perturbation <- function(metamer_list, perturbation) {
  metamer_list$clone()$set_perturbation(perturbation)
}
#' @export
#' @rdname set_params
set_start_probability <- function(metamer_list, start_probability) {
  metamer_list$clone()$set_start_probability(start_probability)
}
#' @export
#' @rdname set_params
set_K <- function(metamer_list, K) {
  metamer_list$clone()$set_start_probability(K)
}
#' @export
#' @rdname set_params
set_change <- function(metamer_list, change) {
  metamer_list$clone()$set_change(change)
}


#' @export
#' @importFrom utils tail
tail.metamer_list <- function(x, n = 1, ...) {
  tail(x$metamers, 1)[[1]]
}

#' @export
#' @importFrom utils head
head.metamer_list <- function(x, n = 1, ...) {
  head(x$metamers, 1)[[1]]
}
