% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mean_reversion.R
\name{passing_time_ou}
\alias{passing_time_ou}
\title{Simulate a set of Ornstein-Uhlenbeck processes in order to generate
distributions for stopping/passing times.}
\usage{
passing_time_ou(
  mu,
  sigma,
  theta,
  start_value,
  passing_value,
  n_simulations,
  seed = NULL
)
}
\arguments{
\item{mu}{The long-run mean of the process.}

\item{sigma}{The instantaneous volatility.}

\item{theta}{The mean reversion parameter.}

\item{start_value}{Value from which we expect to revert over time.}

\item{seed}{Optional seed for random number generation.}
}
\value{
A vector of first passing times. If any of the simulations
        did not result in a pass, NA elements are possible.
}
\description{
Simulate a set of Ornstein-Uhlenbeck processes in order to generate
distributions for stopping/passing times.
}
\examples{
library(FIRVr)
hist(passing_time_ou(0, 5, 0.05, 25, 0, 1000), probability = TRUE)
}
