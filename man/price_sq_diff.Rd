% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/yield_curve_fit.R
\name{price_sq_diff}
\alias{price_sq_diff}
\title{Calculate the squared difference for modelled and market prices.}
\usage{
price_sq_diff(
  parameters,
  prices,
  cashflows,
  zero_rate_model,
  current_date = Sys.Date()
)
}
\arguments{
\item{parameters}{Vector of parameters for the Nelson-Siegel model
(beta, beta1, beta2, tau1).}

\item{prices}{A data frame containing dirty prices for bonds, with
columns Desc and DirtyPrice.}

\item{cashflows}{Data frame of cashflows with columns Data, Amount, Desc.}

\item{zero_rate_model}{A function that can generate zero rates given
parameters and a time to maturity.}

\item{current_date}{The date for which pricing takes place.}
}
\value{
A data frame with columns Desc and ModelDirtyPrice.
}
\description{
Market prices are passed as a parameter, along with cashflows,
a zero rate model function and the parameters for that model.
}
\examples{
library(FIRVr)
cashflows <- tibble(
  Date = as.Date(c("2027-08-15", "2028-02-15", "2028-08-15",
                   "2029-02-15", "2029-02-15")),
  Amount = c(2.5, 2.5, 2.5, 2.5, 100),
  Desc = rep("Bond 1", 5)
)
prices <- tibble(Desc = "Bond 1", DirtyPrice = 80)
price_sq_diff(c(0.034, 0.834, -0.941, 0.957),
              prices,
              cashflows,
              zero_coupon_nelson_siegel,
              "2019-04-16")
}
