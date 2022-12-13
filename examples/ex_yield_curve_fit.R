
library(FIRVr)
library(tidyverse)

#
# Generate some bond data
#

CURRENT_DATE <- "2019-04-16"

bond_1 <- price_treasury_bond("2027-11-15", 0.06125, "1970-01-01", price = 127.359)
bond_2 <- price_treasury_bond("2028-08-15", 0.05500, "1970-01-01", price = 124.156)
bond_3 <- price_treasury_bond("2028-11-15", 0.05250, "1970-01-01", price = 122.562)
bond_4 <- price_treasury_bond("2029-02-15", 0.05250, "1970-01-01", price = 123.000)

bond_1_cf <- bond_1$cashFlow %>% as_tibble() %>% mutate(Desc = "Bond 1")
bond_2_cf <- bond_2$cashFlow %>% as_tibble() %>% mutate(Desc = "Bond 2")
bond_3_cf <- bond_3$cashFlow %>% as_tibble() %>% mutate(Desc = "Bond 3")
bond_4_cf <- bond_4$cashFlow %>% as_tibble() %>% mutate(Desc = "Bond 4")

future_cashflows <- bind_rows(bond_1_cf, bond_2_cf, bond_3_cf, bond_4_cf) %>%
  filter(Date > CURRENT_DATE)

prices <- tibble(
  Desc = c("Bond 1", "Bond 2", "Bond 3", "Bond 4"),
  DirtyPrice = c(bond_1$dirtyPrice, bond_2$dirtyPrice, bond_3$dirtyPrice, bond_4$dirtyPrice)
)

#
# Fit a discount factor curve
#

opt <- optim(
  c(0.05, -0.01, 0.01, 1),
  price_sq_diff,
  prices = prices,
  cashflows = future_cashflows,
  current_date = CURRENT_DATE,
  zero_rate_model = zero_coupon_nelson_siegel,
  method = "BFGS"
)

cat(paste0("Convergence status: ", opt$convergence, "\n"))
cat(paste0("Fitted parameters:  ", paste0(sprintf("%0.3f", opt$par), collapse = ", ")))

fitted_zero_curve <- tibble(
  Time = seq(8, 12, 0.1),
  ZeroRate =  zero_coupon_nelson_siegel(Time, parameters = opt$par)
)

p <- fitted_zero_curve %>%
  ggplot(aes(x =Time, y = ZeroRate)) +
  geom_line() +
  labs(x = "", y = "", title = "Zero rate curve") +
  theme_minimal()
plot(p)

