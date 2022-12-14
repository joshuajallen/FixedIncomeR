% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/db_list_membership.R
\name{db_lm_create_list}
\alias{db_lm_create_list}
\title{Add a new membership list to the database.}
\usage{
db_lm_create_list(conn, list_name, list_desc)
}
\arguments{
\item{conn}{A database connection.}

\item{list_name}{The unique name of the list to be created.}

\item{list_desc}{A description of the list.}
}
\description{
Add a new membership list to the database.
}
\examples{
See FIRVr/examples/ex_db_list_membership.R
}
