dist_2d_euclid <- function(data, target) {
  mean(vapply(seq_len(nrow(data)), function(i) {
    sqrt(min(with(data[i, ], (x - target$x)^2 + (y - target$y)^2)))
  }, 1))
}


dist_euc <- function(data, target) {
  mean(FNN::get.knnx(as.matrix(target), as.matrix(data), k = 1)$nn.dist)
}


# From https://stackoverflow.com/questions/35194048/using-r-how-to-calculate-the-distance-from-one-point-to-a-line
.dist2d <- function(a, b, c) {
  v1 <- b - c
  v2 <- a - b
  m <- cbind(v1,v2)
  abs(det(m))/sqrt(sum(v1*v1))
}

dist2line <- function(data, lines) {
  mean(vapply(1:nrow(lines), function(t) {
    a <-  c(t(lines[t, 1:2]))
    b <- c(t(lines[t, 3:4]))
    min(vapply(1:nrow(data), function(i) {
      .dist2d(c(t(data[i, 1:2])), a, b)
    }, FUN.VALUE = 1))
  }, FUN.VALUE = 1))
}

