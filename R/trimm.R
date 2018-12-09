#' Trimm a `metamer_list`
#'
#' When creating metamers, `metamerize()` can produce thousands of very similar
#' metamers. This function is intended to keep only a subset of them for easier
#' and faster handling and plotting.
#'
#'
#' @param object A `metamer_list` object returned by `metamerize()`
#' @param n The number of metamers to keep.
#'
#' @return
#' A `metamer_list` object with `n` metamers.
#'
#' @export
keep <- function(object, n = length(object)) {
  N <- length(object)
  new <- object[seq(1, N, length.out = n)]
  mostattributes(new) <- attributes(object)
  new
}
