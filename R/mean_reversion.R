
#' Generate Ornstein-Uhlenbeck mean reverting processes
#'
#' Generates series based on the stochastic differential equation
#' \eqn{dx_{t} = \theta(\mu - x_{t}) + \sigma dW_{t}}.
#' \code{generate_ou()} can be used to generate one or many
#' timeseries.
#'
#' @param mu The long-run mean of the process.
#' @param sigma The instantaneous volatility.
#' @param theta The mean reversion parameter.
#' @param start_value Initial value for the series.
#' @param n_periods The number of discrete periods for which to generate.
#' @param n_series The number of timeseries to generate.
#' @param seed Optional seed for random generation
#' @return A tibble with columns Time int, Series and Spread.
#' @export
#' @examples
#' library(FIRVr)
#' df = generate_ou(n_series = 3)
#' @importFrom magrittr "%>%"
#'

generate_ou <- function(mu = 5,
                        sigma = 5,
                        theta = 0.05,
                        start_value = 5,
                        n_periods = 252,
                        n_series = 1,
                        seed = NULL) {

  stopifnot(is.numeric(mu))
  stopifnot(length(mu) == 1)

  stopifnot(is.numeric(sigma))
  stopifnot(length(sigma) == 1)
  stopifnot(sigma > 0)

  stopifnot(is.numeric(theta))
  stopifnot(length(theta) == 1)
  stopifnot(sigma > 0)

  stopifnot(is.numeric(start_value))
  stopifnot(length(start_value) == 1)

  stopifnot(is.numeric(n_periods))
  stopifnot(length(n_periods) == 1)
  stopifnot(n_periods > 0)

  stopifnot(is.numeric(n_series))
  stopifnot(length(n_series) == 1)
  stopifnot(n_series > 0)

  if ( ! is.null(seed) ) {
    set.seed(seed)
  }

  simulated_series <- sde::sde.sim(
    M = n_series,
    model = "OU",
    t0 = 1,
    T = n_periods,
    X0 = start_value,
    theta = c(theta * mu, theta, sigma),
    N = n_periods
  ) %>%
    tibble::as_tibble() %>%
    dplyr::rename_all(function(.){seq_len(n_series)}) %>%
    dplyr::mutate(Time = dplyr::row_number()) %>%
    tidyr::gather(key = "Series", value = "Value", -Time) %>%
    dplyr::select(Series, Time, Value)

}


#' Fit an Ornstein-Uhlenbeck model to a timeseries.
#'
#' A maximum likelihood estimate is made of the parameters
#' mu, theta and sigma of the model:
#' \eqn{dx_{t} = \theta(\mu - x_{t}) + \sigma dW_{t}}.
#'
#' Optimisation is performed by optim, and the conversion status of
#' the process can be checked using the convergence value in the
#' returned list.
#'
#' @param series A vector of the timeseries to be fitted.
#' @return A list of the model parameters, their errors and outputs from the optimisation.
#' @export
#' @examples
#' library(FIRVr)
#' params = fit_ou(generate_ou()$Value)
#' @seealso \code{\link{generate_ou}}

fit_ou <- function(series) {

  stopifnot(is.numeric(series))
  stopifnot(length(series) > 1)

  ou_fit <- stats4::mle(
    FIRVr:::ou_lik(series),
    start = list(theta1 = 1, theta2 = 1, theta3 = 1),
    method = "L-BFGS-B",
    lower = c(-Inf, 1e-6, 1e-6)
  )

  mle_summary <- stats4::summary(ou_fit)

  results <- FIRVr:::sde_ou_coeffs_to_preferred_format(mle_summary@coef)
  results[["convergence"]] <- ou_fit@details$convergence
  results[["message"]] <- ou_fit@details$message
  results[["value"]] <- ou_fit@details$value

  results

}


#' Generate a likelihood function to be used with e.g. mle() when
#' fitting an Ornstein-Uhlenbeck model to a timeseries.
#'
#' @param x A timeseries of values for which O-U parameters are to be estimated.
#'

ou_lik <- function(x) {
  function(theta1, theta2, theta3) {
    n <- length(x)
    -sum(
      sde::dcOU(
        x = x[2:n],
        Dt = 1,
        x0 = x[1:(n-1)],
        theta = c(theta1, theta2, theta3),
        log = TRUE
      )
    )
  }
}


#' Calculate conditional densities for an Ornstein-Uhlenbeck
#' mean reversion model.
#'
#' @param mu The long-run mean of the process.
#' @param sigma The instantaneous volatility.
#' @param theta The mean reversion parameter.
#' @param start_value Value from which we expect to revert over time.
#' @param horizons Vector of N_business_days for which we want densities.
#' @param seed Optional seed for random number generation.
#' @return A tibble of Horizon, Spread and Density.
#' @export
#' @examples
#' library(FIRVr)
#' densities = conditional_densities_ou(5, 5, 0.05, 20, c(5, 21))
conditional_densities_ou <- function(mu,
                                     sigma,
                                     theta,
                                     start_value,
                                     horizons,
                                     seed = NULL) {

  stopifnot(is.numeric(mu))
  stopifnot(length(mu) == 1)

  stopifnot(is.numeric(sigma))
  stopifnot(length(sigma) == 1)
  stopifnot(sigma > 0)

  stopifnot(is.numeric(theta))
  stopifnot(length(theta) == 1)
  stopifnot(sigma > 0)

  stopifnot(is.numeric(start_value))
  stopifnot(length(start_value) == 1)

  stopifnot(is.numeric(horizons))

  # Generate a series based on the parameters provided
  # in order to determine sensible target values.
  test_series <- generate_ou(mu = 5,
                             sigma = 5,
                             theta = 0.05,
                             start_value = mu,
                             n_periods = 252,
                             n_series = 1,
                             seed = seed)$Value

  n_steps <- 100
  increment <- 1.5 * (max(test_series) - min(test_series)) / n_steps
#  target_values <-
#    (min(test_series) - 0.2 * n_steps * increment) +
#      1:(1.4*n_steps) * increment

  target_values <- seq(start_value - increment * n_steps/2,
                       start_value + increment * n_steps/2,
                       by = increment)

  conditional_density_list <- list()

  for (horizon in horizons) {
    densities <- sde::dcOU(
      x = target_values,
      Dt = horizon,
      x0 = rep(start_value, length(target_values)),
      theta = c(theta * mu, theta, sigma),
      log = FALSE
    )
    conditional_density_list[[as.character(horizon)]] <-
      tibble::tibble(
        Horizon = horizon,
        Value = target_values,
        Density = densities
      )
  }

  dplyr::bind_rows(conditional_density_list)

}


#' Calculate the half-life of an Ornstein-Uhlenbeck process
#'
#' @param theta The estimated theta coefficient for the process
#' @export
#' @return The half-life.
half_life_ou <- function(theta) {
  log(2, base = exp(1)) / theta
}


#' Convert sde O-U theta coefficients to preferred format.
#' That is: theta1, theta2, theta3 -> mu, theta, sigma.
#'
#' @param sde_theta_coef A data frame of coefficients and standard errors
#' @return A list of parameters and estimated errors in our preferred format.
sde_ou_coeffs_to_preferred_format <- function(sde_theta_coef) {

  list(
    "theta" = sde_theta_coef["theta2", "Estimate"],
    "theta_stderr" = sde_theta_coef["theta2", "Std. Error"],
    "mu" = sde_theta_coef["theta1", "Estimate"] /
      sde_theta_coef["theta2", "Estimate"],
    # Assume errors add in quadrature:
    # err_theta1^2 = err_theta2^2 + err_mu^2
    # err_mu = sqrt( err_theta1^2 - err_theta2^2 )
    "mu_stderr" = sqrt( sde_theta_coef["theta1", "Std. Error"]^2 -
                          sde_theta_coef["theta2", "Std. Error"]^2 ),
    "sigma" = sde_theta_coef["theta3", "Estimate"],
    "sigma_stderr" = sde_theta_coef["theta3", "Std. Error"]
  )

}


#' Simulate a set of Ornstein-Uhlenbeck processes in order to generate
#' distributions for stopping/passing times.
#'
#' @param mu The long-run mean of the process.
#' @param sigma The instantaneous volatility.
#' @param theta The mean reversion parameter.
#' @param start_value Value from which we expect to revert over time.
#' @param seed Optional seed for random number generation.
#' @return A vector of first passing times. If any of the simulations
#'         did not result in a pass, NA elements are possible.
#' @export
#' @examples
#' library(FIRVr)
#' hist(passing_time_ou(0, 5, 0.05, 25, 0, 1000), probability = TRUE)
passing_time_ou <- function(mu,
                            sigma,
                            theta,
                            start_value,
                            passing_value,
                            n_simulations,
                            seed = NULL) {

  # Generate simulations based on the parameters provided
  # in order to determine distribution of outcomes.

  sim_series <- generate_ou(mu = mu,
                            sigma = sigma,
                            theta = theta,
                            start_value = start_value,
                            n_periods = 1000,
                            n_series = n_simulations,
                            seed = seed)

  if ( passing_value < start_value ) {
    sim_series <- sim_series %>% dplyr::mutate(Passed = Value < passing_value)
  } else {
    sim_series <- sim_series %>% dplyr::mutate(Passed = Value > passing_value)
  }

  first_passes <- sim_series %>%
    dplyr::filter(Passed == TRUE) %>%
    dplyr::group_by(Series) %>%
    dplyr::slice(1)

  # Populate a vector, leaving NAs where no pass occurred
  first_passing_times <- rep(NA, n_simulations)
  first_passing_times[as.integer(first_passes$Series)] <- first_passes$Time

  first_passing_times

}
