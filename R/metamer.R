metamerise <- function(data, FUN, cols, signif = 2,  N = 100, target = NULL, distance_fun,
                       annealing = FALSE,...) {
  data <- as.data.frame(data)
  fun <- match.fun(FUN)
  org <- fun(data)

  ncols <- length(cols)
  nrows <- nrow(data)
  npoints <- ncols*nrows

  # steps <- list(length = N)

  R <- mean(c(diff(range(data$x)), diff(range(data$y))))

  steps <- vector(mode = "list", length = N)
  on.exit(return(data.table::rbindlist(steps, idcol = TRUE)))
  m <- 1
  steps[[m]] <- data
  new_data <- data
  dist_new <- NULL

  M_temp <- 0.4
  m_temp <- 0.01
  i <- 1

  tries <- 1
  max_tries <- 1000
  ann <- FALSE

  # Calculate distance
  if (!is.null(target)) {
    dist_org <- dist <- distance_fun(new_data[, c(cols)], target)
  }

  while (i <= N && tries <= max_tries) {

    temp <- M_temp + ((i-1)/(N-1))^2*(m_temp - M_temp)
    # Select point to change
    # point <- sample(1:nrow(new_data), nrow(new_data))

    # Perturb data
    new_data[, c(cols)] <- data[, c(cols)] + matrix(rnorm(npoints, 0, 0.05),
                                                    nrow = nrows, ncol = ncols)
    new <- fun(new_data)

    if (all(signif(new, signif) - signif(org, signif) == 0)) {
      if (!is.null(target)) {
        dist_new <- distance_fun(new_data[, c(cols)], target)
      }

      if (isTRUE(annealing)) ann <- temp > runif(1)

      if (is.null(target) || dist_new <= dist || ann ) {

        # If distance is smaller or is not needed

        # Save data and state
        m <- m + 1
        steps[[m]] <- new_data
        data <- new_data
        cat("Found the ", m, "th Metamer in the ", i, "th iteration with distance: ", dist_new, " (", dist_new/dist_org, ")\r",
            sep = "")
        dist <- dist_new
      }
    }
    i <- i + 1
  }
  data.table::rbindlist(steps, idcol = TRUE)
}
