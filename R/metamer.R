
metamer <- function(data, FUN, cols, signif = 2,  N = 100, target = NULL, distance_fun,
                    annealing = FALSE,...) {
  fun <- match.fun(FUN)
  org <- fun(data)

  change <- dim(data[, c(cols)])

  steps <- list()


  new_data <- data
  dist_new <- NULL

  M_temp <- 0.4
  m_temp <- 0.01
  i <- 1
  tries <- 1
  max_tries <- 10000
  ann <- FALSE

  # Calculate distance
  if (!is.null(target)) {
    dist_org <- dist <- distance_fun(new_data[, c(cols)], target)
  }

  while (i <= N && tries <= max_tries) {
    temp <- M_temp + (i-1)/N*(m_temp - M_temp)
    # Select point to change
    point <- sample(1:nrow(new_data), nrow(new_data))

    # Perturb data
    new_data[point, c(cols)] <- data[point, c(cols)] + rnorm(change[2], 0, 0.1/tries)
    new <- fun(new_data)

    # If statistics are close
    if (all(round(new, signif) - round(org, signif) == 0)) {
      # Compute new distance (if needed)
      if (!is.null(target)) {
        dist_new <- distance_fun(new_data[, c(cols)], target)
      }

      # If distance is smaller or is not needed
      # if (isTRUE(annealing)) ann <- temp > runif(1)
      if (is.null(target) || dist_new <= dist || ann ) {
        # Save data and state
        steps[[i]] <- new_data
        data <- new_data

        cat("Tried ", tries, " times and found the ", i, "th metamer with distance: ", dist_new, " (", dist_new/dist_org, ")\r",
            sep = "")
        dist <- dist_new
        i <- i + 1
        tries <- 1
      }
    } else {
      tries <- tries + 1
    }
  }
  data.table::rbindlist(steps, idcol = TRUE)
}
