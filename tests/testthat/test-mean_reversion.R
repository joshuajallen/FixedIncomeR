#
# Unit tests for the mean reversion functions
#

context("test-mean_reversion")

test_that("Seeded OU generator output is as expected", {
  expect_equal(as.data.frame(
    generate_ou(
      mu = 5,
      sigma = 5,
      theta = 0.05,
      start_value = 5,
      n_periods = 3,
      n_series = 2,
      seed = 0
    )
  ),
  as.data.frame(tibble::tibble(
    Series = c(rep("1", 4), rep("2", 4)),
    Time = rep(seq(1, 4), 2),
    Value = c(
      5.000000,
      10.071238,
      15.244628,
      16.573711,
      5.000000,
      3.690050,
      8.842279,
      2.532834
    )
  )),
  tolerance = 1e-6)
})

test_that("Half-life estimate is reasonable", {
  expect_equal(
    half_life_ou(0.5),
    1.386294,
    tolerance = 1e-6
  )
})

test_that("OU process fitting converges", {
  expect_equal(
    as.numeric(
      fit_ou(c(1,2,3,4,5,6,7,6,5,4,3,2,1))[
        c("theta", "theta_stderr", "mu", "sigma")
      ]
    ),
    c(0.1718496, 0.1848767, 4.000003, 1.043253),
    tolerance = 1e-6
  )
})

test_that("Conditional density can be calculated", {
  expect_equal(
    as.data.frame(
      conditional_densities_ou(5, 5, 0.05, 20, c(5, 21), 0)[c(40, 60, 140), ]
    ),
    data.frame(
      Horizon = c(5, 5, 21),
      Value = c(11.77255, 26.73155, 11.02460),
      Density = c(0.03558585, 0.02407358, 0.02689741)
    ),
    tolerance = 1e-6
  )
})

test_that("Passing time distribution is as expected", {
  expect_equal(
    passing_time_ou(5, 5, 0.05, 20, 0, 100, 0)[1:10],
    c(11, 43, 99, 54, 20, 10, 65, 92, 28, 21),
    tolerance = 1e-6
  )
})
