mean_dist_to <- function(target) {
  force(target)
  function(data) {
    mean(FNN::get.knnx(as.matrix(target), as.matrix(data), k = 1)$nn.dist)
  }
}


