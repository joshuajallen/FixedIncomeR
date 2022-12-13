
# TODO: Test method for this when up and running

#' Fit piecewise bond curve using Quantlib
#'
#' @param maturityDates Maturity dates of the bonds
#' @param cleanPrices Clean prices of the bonds
#' @param coupons Coupons in decimal format
#' @param startDate Target date for analysis (defaults to today)
#' @param config A list of configuration options:
#'
#' couponFrequency: QuantLib frequency such as Annual or SemiAnnual
#'
#' settlementDays: e.g. 2
#'
#' forwardPeriodMonths: Forward period in months for forward rates
#'
#' dayCountConvention: QuantLib day count convention such as ActualActual
#'
#' businessDayConvention: Quantlib business day convention such as ModifiedFollowing
#'
#' trait: Quantlib trait for fitting. May be Discount, ForwardRate or ZeroYield
#'
#' interp: Quantlib interpolation type. May be Linear, LogLinear or Cubic
#'
#' @return A tibble of Date, ZeroRate, Discount and Forward
#'
#' @export
#' @importFrom Rcpp evalCpp
#' @useDynLib FIRVr
#' @examples
#'
#'  fit_piecewise_bond_curve(
#'     c(as.Date("2022-06-01"),
#'       as.Date("2025-06-01"),
#'       as.Date("2030-06-01"),
#'       as.Date("2050-06-01")),
#'     c(102, 101, 100, 99),
#'     c(0, 0, 0, 0),
#'     startDate = Sys.Date(),
#'     config = list(
#'       "couponFrequency" = "Annual",
#'       "settlementDays" = 2,
#'       "forwardPeriodMonths" = 3,
#'       "dayCountConvention" = "ActualActual",
#'       "businessDayConvention" = "ModifiedFollowing",
#'       "trait" = "Discount",
#'       "interp" = "LogLinear"
#'     )
#'   )
#'
#'
fit_piecewise_bond_curve <- function(
  maturityDates,
  cleanPrices,
  coupons,
  startDate = Sys.Date(),
  config) {

  df <- fitPiecewiseBondCurve(maturityDates,
                              cleanPrices,
                              coupons,
                              startDate,
                              config)
  tibble::as_tibble(df)

}

