
library(FIRVr)
library(tidyverse)


#
# Custom palette for plotting
#

CUSTOM_PAL_LINES <- c("#2c7bb6", "#F9543A", "#4DAF4A", "#984EA3",
                      "#FF7F00", "#c1c1c1", "#A65628", "#F781BF")


#
# Generate a test series
#

series <- generate_ou(mu = 5,
                      sigma = 5,
                      theta = 0.05,
                      start_value = 0,
                      n_periods = 252,
                      n_series = 1)


#
# Fit to extract process parameters
#

fit <- fit_ou(series$Value)
cat(
  paste0(
    "Fit convergence status: ",
    fit$convergence,
    "\n"
  )
)


#
# Estimated half-life
#

cat(
  paste0(
    "Half-life: ",
    sprintf("%0.1f", half_life_ou(fit$theta)),
    "\n"
  )
)


#
# Calculate some conditional densities
#

conditional_densities <- conditional_densities_ou(fit$mu,
                                                  fit$sigma,
                                                  fit$theta,
                                                  20,
                                                  c(5, 21, 63))

p <- conditional_densities %>%
  mutate(Horizon = factor(Horizon)) %>%
  ggplot(aes(x = Value, y = Density)) +
  geom_line(aes(group = Horizon, colour = Horizon)) +
  xlab("") +
  ylab("") +
  ggtitle("Conditional Densities") +
  theme_minimal()  +
  scale_color_manual(values = CUSTOM_PAL_LINES)

plot(p)


#
# Calculate expected passing time
#

cat("Running Monte Carlo for passing times...\n")
passing_times <- passing_time_ou(fit$mu,
                                 fit$sigma,
                                 fit$theta,
                                 start_value = 20,
                                 passing_value = 0,
                                 n_simulations = 1000)

cat(
  paste0(
    "Simulated passing time: ",
    sprintf("%0.1f", mean(passing_times)),
    " ± ",
    sprintf("%0.1f", sd(passing_times)),
    "\n"
  )
)

p <- tibble(Time = seq_along(passing_times), Value = passing_times) %>%
  ggplot(aes(Value)) +
  geom_histogram(aes(y = stat(density)),
                 fill = CUSTOM_PAL_LINES[1],
                 colour = CUSTOM_PAL_LINES[1],
                 bins = 40) +
  xlab("Time") +
  ylab("") +
  ggtitle("Simulated passing time density") +
  theme_minimal()
plot(p)
