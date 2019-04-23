#' @export
as.data.frame.metamer_list <- function(x, ..., n = length(x)) {
  x <- trim(x, n = n)
  n <- nrow(x[[1]])
  df <- do.call(rbind, x)
  df$.metamer <- rep(1:length(x), each = n)
  df$.name <- rep(attr(x, "name"), each = n)
  lc <- length(colnames(df))
  df <- df[, c(lc, 1:(lc-1))]
  attr(df, "convergence") <- attr(x, "convergence")
  return(df)
}


#' @export
print.metamer_list <- function(x, ...) {
  cat("List of ", length(x), " metamers", sep = "")
}


# Dynamically exported. From:
# https://community.rstudio.com/t/write-s3-methods-for-generics-from-other-packages-without-importing-that-package/8884/4
fortify.metamer_list <- function(model, data, ...) {
  as.data.frame(model, ...)
}


.onLoad <- function(...) {
  register_s3_method("ggplot2", "fortify", "metamer_list")
  invisible()
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
