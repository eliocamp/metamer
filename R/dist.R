dist_2d_euclid <- function(data, target) {
  mean(vapply(seq_len(nrow(data)), function(i) {
    min(with(data[i, ], (carat - target$carat)^2 + (price - target$price)^2))
  }, 1))
}
