#' @export
as.data.frame.metamer_list <- function(x, ...) {
  n <- nrow(x[[1]])
  df <- do.call(rbind, x)
  df$.metamer <- rep(1:length(x), each = n)
  lc <- length(colnames(df))
  df <- df[, c(lc, 1:(lc-1))]
  attr(df, "convergence") <- attr(x, "convergence")
  return(df)
}


#' @export
print.metamer_list <- function(x, ...) {
  cat("List of ", length(x), " metamers", sep = "")
}
