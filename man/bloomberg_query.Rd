% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bloomberg.R
\name{bloomberg_query}
\alias{bloomberg_query}
\title{Query Bloomberg for timeseries data.}
\usage{
bloomberg_query(
  securities,
  fields,
  from_date = Sys.Date() - 365,
  to_date = Sys.Date(),
  options = NULL
)
}
\arguments{
\item{securities}{A list of Bloomberg security IDs}

\item{fields}{Timeseries fields}

\item{from_date}{Start date}

\item{to_date}{End date}

\item{options}{A named list of options to pass to Bloomberg}
}
\value{
A data frame of tidy data with columns Security, Date, Field and Value
}
\description{
Query Bloomberg for timeseries data.
}
\examples{
bloomberg_query("DE0001135176 Govt", "PX_LAST",
  from_date = "2019-05-08", to_date = "2019-05-10")
}
