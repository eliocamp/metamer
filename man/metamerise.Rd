% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metamerize.R
\name{metamerise}
\alias{metamerise}
\alias{metamerize}
\alias{new_metamer}
\title{Create metamers}
\usage{
metamerise(
  data,
  preserve,
  minimize = NULL,
  change = colnames(data),
  round = truncate_to(2),
  stop_if = n_tries(100),
  keep = NULL,
  annealing = TRUE,
  K = 0.02,
  start_probability = 0.5,
  perturbation = 0.08,
  name = "",
  verbose = interactive()
)

metamerize(
  data,
  preserve,
  minimize = NULL,
  change = colnames(data),
  round = truncate_to(2),
  stop_if = n_tries(100),
  keep = NULL,
  annealing = TRUE,
  K = 0.02,
  start_probability = 0.5,
  perturbation = 0.08,
  name = "",
  verbose = interactive()
)

new_metamer(data, preserve, round = truncate_to(2))
}
\arguments{
\item{data}{A \code{data.frame} with the starting data or a \code{metamer_list} object returned
by a previous call to the function.}

\item{preserve}{A function whose result must be kept exactly the same.
Must take the data as argument and return a numeric vector.}

\item{minimize}{An optional function to minimize in the process. Must take
the data as argument and return a single numeric.}

\item{change}{A character vector with the names of the columns that need to be
changed.}

\item{round}{A function to apply to the result of \code{preserve} to round
numbers. See \link{truncate_to}.}

\item{stop_if}{A stopping criterium. See \link{n_tries}.}

\item{keep}{Max number of metamers to return.}

\item{annealing}{Logical indicating whether to perform annealing.}

\item{K}{speed/quality tradeoff parameter.}

\item{start_probability}{initial probability of rejecting bad solutions.}

\item{perturbation}{Numeric with the magnitude of the random perturbations.
Can be of length 1 or \code{length(change)}.}

\item{name}{Character for naming the metamers.}

\item{verbose}{Logical indicating whether to show a progress bar.}
}
\value{
A \code{metamer_list} object (a list of data.frames).
}
\description{
Produces very dissimilar datasets with the same statistical properties.
}
\details{
It follows Matejka & Fitzmaurice (2017) method of constructing metamers.
Beginning from a starting dataset, it iteratively adds a small perturbation,
checks if \code{preserve} returns the same value (up to \code{signif} significant digits)
and if \code{minimize} has been lowered, and accepts the solution for the next
round. If \code{annealing} is \code{TRUE}, it also accepts solutions with bigger
\code{minimize} with an ever decreasing probability to help the algorithm avoid
local minimums.

The annealing scheme is adapted from de Vicente et al. (2003).

If \code{data} is a \code{metamer_list}, the function will start the algorithm from the
last metamer of the list. Furthermore, if \code{preserve} and/or \code{minimize}
are missing, the previous functions will be carried over from the previous call.

\code{minimize} can be also a \emph{vector} of functions. In that case, the process minimizes
the product of the functions applied to the data.
}
\examples{
data(cars)
# Metamers of `cars` with the same mean speed and dist, and correlation
# between the two.
means_and_cor <- delayed_with(mean_speed = mean(speed),
                              mean_dist = mean(dist),
                              cor = cor(speed, dist))
set.seed(42)  # for reproducibility.
metamers <- metamerize(cars,
                       preserve = means_and_cor,
                       round = truncate_to(2),
                       stop_if = n_tries(1000))
print(metamers)

last <- tail(metamers)

# Confirm that the statistics are the same
cbind(original = means_and_cor(cars),
      metamer = means_and_cor(last))

# Visualize
plot(tail(metamers))
points(cars, col = "red")

}
\references{
Matejka, J., & Fitzmaurice, G. (2017). Same Stats, Different Graphs. Proceedings of the 2017 CHI Conference on Human Factors in Computing Systems  - CHI ’17, 1290–1294. https://doi.org/10.1145/3025453.3025912
de Vicente, Juan, Juan Lanchares, and Román Hermida. (2003). ‘Placement by Thermodynamic Simulated Annealing’. Physics Letters A 317(5): 415–23.
}
\seealso{
\code{\link[=delayed_with]{delayed_with()}} for a convenient way of making functions suitable for
\code{preserve}, \code{\link[=mean_dist_to]{mean_dist_to()}} for a convenient way of minimizing the distance
to a known target in \code{minimize}, \code{\link[=mean_self_proximity]{mean_self_proximity()}} for maximizing the
"self distance" to prevent data clumping.
}
