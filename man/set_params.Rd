% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functionals.R
\name{clear_minimize}
\alias{clear_minimize}
\alias{clear_minimise}
\alias{set_minimise}
\alias{set_minimize}
\alias{get_last_metamer}
\alias{set_annealing}
\alias{set_perturbation}
\alias{set_start_probability}
\alias{set_K}
\alias{set_change}
\title{Set metamer parameters}
\usage{
clear_minimize(metamer_list)

clear_minimise(metamer_list)

set_minimise(metamer_list, minimize)

set_minimize(metamer_list, minimize)

get_last_metamer(metamer_list)

set_annealing(metamer_list, annealing)

set_perturbation(metamer_list, perturbation)

set_perturbation(metamer_list, perturbation)

set_start_probability(metamer_list, start_probability)

set_K(metamer_list, K)

set_change(metamer_list, change)
}
\arguments{
\item{metamer_list}{A \code{metamer_list} object.}

\item{minimize}{An optional function to minimize in the process. Must take
the data as argument and return a single numeric.}

\item{annealing}{Logical indicating whether to perform annealing.}

\item{perturbation}{Numeric with the magnitude of the random perturbations.
Can be of length 1 or \code{length(change)}.}

\item{start_probability}{initial probability of rejecting bad solutions.}

\item{K}{speed/quality tradeoff parameter.}

\item{change}{A character vector with the names of the columns that need to be
changed.}
}
\description{
Set metamer parameters
}
