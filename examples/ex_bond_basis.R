
library(FIRVr)
library(tidyverse)

#
# Generate some highly correlated series to serve as test data
#

df <- generate_correlated_series(
  means  = c(0.02672, 0.02754, 0.02758, 0.02801),
  stdevs = c(0.00070, 0.00070, 0.00070, 0.00070),
  cor_mat = matrix(
    c(1.00, 0.93, 0.91, 0.89,
      0.93, 1.00, 0.92, 0.90,
      0.91, 0.92, 1.00, 0.91,
      0.89, 0.90, 0.91, 1.00),
    ncol = 4,
    dimnames = list(c("Bond A", "Bond 2", "Bond 3", "Bond X"), NULL)
  ),
  field_descs = "Yield"
)

p <- df %>%
  ggplot2::ggplot(ggplot2::aes(x = Date, y = Value, color = Desc)) +
  ggplot2::geom_line() +
  ggplot2::labs(x = "", y = "", title = "Yield") +
  ggplot2::theme_minimal()
plot(p)


#
# Define some data for our CTD basket of bonds
#

static_data <- readr::read_csv(
    "Desc,MaturityDate,Coupon,EffectiveDate,CF
    Bond A,2017-11-15,0.04250,2007-11-15,0.9069
    Bond 2,2017-11-30,0.02250,2015-11-30,0.8136
    Bond 3,2018-02-15,0.03500,2007-02-15,0.8628
    Bond X,2018-05-15,0.03875,2006-05-15,0.8800"
  )

forward_prices <- readr::read_csv(
  "Desc,ForwardPrice
  Bond A,110.49687
  Bond 2,98.93279
  Bond 3,106.19214
  Bond X,107.71037"
)

future_price <- 121.0


#
# Transform data into one level series and n-1 spread series
#

level_and_spread_series <- to_level_and_spreads(df)


#
# Extract parameters for use in Monte Carlo from the historical data
#

model_params <- extract_bond_basis_params(level_and_spread_series)


#
# Run Monte Carlo to identify many possible end states
#

bond_basis_model <- bond_basis_model(model_params$means,
                                     model_params$stdevs,
                                     model_params$correlations,
                                     model_params$field_descs,
                                     static_data,
                                     forward_prices,
                                     future_price,
                                     evaluation_date = "2010-12-30",
                                     n_periods = 63,
                                     n_simulations = 1000)


#
# Inspect the simulated yields
#

p <- bond_basis_model$simulated_yields %>%
  dplyr::filter(Simulation <= 10) %>%
  ggplot2::ggplot(ggplot2::aes(x = Date, y = Value, color = Desc,
                               group = interaction(Simulation, Desc))) +
  ggplot2::geom_line() +
  ggplot2::labs(x = "", y = "", title = "Simulated yield") +
  ggplot2::theme_minimal()
plot(p)


#
# Inspect the forward converted prices
#

p <- bond_basis_model$model_output %>%
  ggplot2::ggplot(ggplot2::aes(x = FwdConvertedPrice)) +
  ggplot2::geom_histogram(
    ggplot2::aes(fill = Desc),
    color = "grey",
    size = 0.25,
    binwidth = 0.05,
    position = "identity",
    alpha = 0.5
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "", y = "", title = "FwdConvertedPrice histogram")
plot(p)


#
# Probabilities of being the CTD
#

p <- bond_basis_model$model_output %>%
  dplyr::group_by(Desc) %>%
  dplyr::summarise(ProbabilityCTD = sum(IsCTD)/dplyr::n()) %>%
  dplyr::arrange(desc(ProbabilityCTD)) %>%
  dplyr::mutate(Desc = factor(Desc, levels = Desc)) %>%
  ggplot2::ggplot(ggplot2::aes(x = Desc, y = ProbabilityCTD, fill = Desc)) +
  ggplot2::geom_bar(stat = "identity") +
  ggplot2::labs(x = "", y = "", title = "Probability of being CTD") +
  ggplot2::theme_minimal()
plot(p)


#
# DO valuation
#

p <- bond_basis_model$DO_valuation %>%
  ggplot2::ggplot(ggplot2::aes(x = Payoff, stat(density))) +
  ggplot2::geom_histogram(
    fill = "lightsteelblue",
    binwidth = 0.001
  ) +
  ggplot2::theme_minimal() +
  ggplot2::labs(x = "", y = "", title = "DO valuation")
plot(p)
