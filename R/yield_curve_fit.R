#' Generate zero coupon yields from a Nelson-Siegel forward rate model.
#'
#' @param t Time in years for required zero coupon rate.
#' @param parameters Vector of parameters for the Nelson-Siegel model
#'        (beta, beta1, beta2, tau1)
#' @return A zero coupon yield in decimal format
#' @export
#' @examples
#' library(FIRVr)
#' zero_coupon_nelson_siegel(1, c(0.034, 0.834, -0.941, 0.957))
zero_coupon_nelson_siegel <- function(t, parameters) {

  # Note: We could expand the Nelson-Siegel model with an additional
  #       term to get the Svensson model (see BoC paper)
  # https://www.bankofcanada.ca/wp-content/uploads/2010/01/tr84.pdf

  beta0 <- parameters[1]
  beta1 <- parameters[2]
  beta2 <- parameters[3]
  tau1  <- parameters[4]

  beta0 +
    beta1 * (  (1 - exp(-t/tau1)) / (t/tau1) ) +
      beta2 * ( ((1 - exp(-t/tau1)) / (t/tau1)) - exp(-t/tau1) )

}


#' Calculate modelled prices for bond cashflows and a forward curve model.
#'
#' @param parameters Vector of parameters for the Nelson-Siegel model
#'        (beta, beta1, beta2, tau1).
#' @param cashflows Data frame of cashflows with columns Data, Amount, Desc.
#' @param zero_rate_model A function that can generate zero rates given
#'        parameters and a time to maturity.
#' @param current_date The date for which the pricing takes place.
#' @return A data frame with columns Desc and ModelDirtyPrice.
#' @export
#' @examples
#' library(FIRVr)
#' cashflows <- tibble(
#'   Date = as.Date(c("2027-08-15", "2028-02-15", "2028-08-15",
#'                    "2029-02-15", "2029-02-15")),
#'   Amount = c(2.5, 2.5, 2.5, 2.5, 100),
#'   Desc = rep("Bond 1", 5)
#' )
#' price_from_curve(c(0.034, 0.834, -0.941, 0.957),
#'                  cashflows,
#'                  zero_coupon_nelson_siegel,
#'                  "2019-04-16")
#' @importFrom magrittr "%>%"
price_from_curve <- function(parameters,
                             cashflows,
                             zero_rate_model,
                             current_date) {

  distinct_dates <- cashflows$Date %>% sort() %>% unique()
  days <- difftime(distinct_dates, current_date, units = "d") %>% as.numeric()
  times <- days / 365.25

  zero_rates <- tibble::tibble(
    Time = times,
    Date = distinct_dates,
    ZeroRate = zero_rate_model(times, parameters)
  ) %>%
    dplyr::mutate(
      DF = exp(-ZeroRate*Time)
    )

  # Join together the zero rates for all cashflows with the pricing data

  cashflows %>%
    dplyr::left_join(zero_rates, by = "Date") %>%
    dplyr::mutate(
      PV = Amount * DF
    ) %>%
    dplyr::group_by(Desc) %>%
    dplyr::summarise(
      ModelDirtyPrice = sum(PV)
    ) %>%
    dplyr::ungroup()

}


#' Calculate the squared difference for modelled and market prices.
#'
#' Market prices are passed as a parameter, along with cashflows,
#' a zero rate model function and the parameters for that model.
#'
#' @param parameters Vector of parameters for the Nelson-Siegel model
#'        (beta, beta1, beta2, tau1).
#' @param prices A data frame containing dirty prices for bonds, with
#'        columns Desc and DirtyPrice.
#' @param cashflows Data frame of cashflows with columns Data, Amount, Desc.
#' @param zero_rate_model A function that can generate zero rates given
#'        parameters and a time to maturity.
#' @param current_date The date for which pricing takes place.
#' @return A data frame with columns Desc and ModelDirtyPrice.
#' @export
#' @examples
#' library(FIRVr)
#' cashflows <- tibble(
#'   Date = as.Date(c("2027-08-15", "2028-02-15", "2028-08-15",
#'                    "2029-02-15", "2029-02-15")),
#'   Amount = c(2.5, 2.5, 2.5, 2.5, 100),
#'   Desc = rep("Bond 1", 5)
#' )
#' prices <- tibble(Desc = "Bond 1", DirtyPrice = 80)
#' price_sq_diff(c(0.034, 0.834, -0.941, 0.957),
#'               prices,
#'               cashflows,
#'               zero_coupon_nelson_siegel,
#'               "2019-04-16")
#' @importFrom magrittr "%>%"
price_sq_diff <- function(parameters,
                          prices,
                          cashflows,
                          zero_rate_model,
                          current_date = Sys.Date()) {

  current_date <- as.Date(current_date)

  price_from_curve(parameters, cashflows, zero_rate_model, current_date) %>%
    dplyr::left_join(prices, by = "Desc") %>%
    dplyr::mutate(SqDifference = (ModelDirtyPrice - DirtyPrice)^2) %>%
    dplyr::select(SqDifference) %>%
    sum()

}
