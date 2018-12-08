
trimm <- function(object, keep = length(object)) {
  N <- length(object)
  new <- object[seq(1, N, length.out = keep)]
  mostattributes(new) <- attributes(object)
  new
}
