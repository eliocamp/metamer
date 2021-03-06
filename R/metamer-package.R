#' @section Overview:
#' Create metamers with the [metamerize()] function.
#'
#' Some helper functions included:
#' * [draw_data()] for drawing 2D datasets by hand and [densify()] for increasing
#' the point density of those drawings.
#'
#' * [delayed_with()] for defining statistics to preserve.
#'
#' * [moments_n()] for preserving moments of order n.
#'
#' * [mean_dist_to()] for minimizing the mean distance to a known target dataset.
#'
#' The [as.data.frame()]/`[data.table::as.data.table()] methods included will turn a `metamer_list` into a tidy
#' data.frame.
#'
#' Inspired by Matejka & Fitzmaurice (2017) awesome paper.
#'
#' @references
#' Matejka, J., & Fitzmaurice, G. (2017). Same Stats, Different Graphs. Proceedings of the 2017 CHI Conference on Human Factors in Computing Systems  - CHI ’17, 1290–1294. https://doi.org/10.1145/3025453.3025912
#'
#' @keywords internal
"_PACKAGE"
