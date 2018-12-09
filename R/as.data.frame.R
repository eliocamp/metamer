#' @export
as.data.frame.metamer_list <- function(x, row.names = NULL, optional = FALSE, ...,
                                       cut.names = FALSE, col.names = names(x), fix.empty.names = TRUE,
                                       stringsAsFactors = default.stringsAsFactors()) {
  n <- nrow(x[[1]])
  df <- do.call(rbind, x)
  df$.metamer <- rep(1:length(x), each = n)
  lc <- length(colnames(df))
  df <- df[, c(lc, 1:(lc-1))]
  attr(df, "convergence") <- attr(x, "convergence")
  return(df)
}

#' @importFrom data.table as.data.table
#' @export
as.data.table.metamer_list <- function(x, keep.rownames = FALSE, ...) {
  df <- data.table::rbindlist(x, idcol = ".metamer")
  attr(df, "convergence") <- attr(x, "convergence")
  return(df)
}
