#
# Unit tests for the bond basis functions
#

context("test-future_basis")

test_that("Accrued interest calculation is correct", {
  expect_equal(
    calculate_accrued_interest(
      settle = "2019-11-29",
      mature = "2031-01-04",
      coupon = 0.055,
      freq = 1,
      convention = "ACT/ACT"
    ) %>%
      as.numeric(),
    4.957534,
    tolerance = 1e-6
  )
})

test_that("Duration calculation is correct", {
  expect_equal(
    calculate_duration(
      settle = "2019-11-29",
      mature = "2031-01-04",
      coupon = 0.055,
      yield = 0,
      freq = 1,
      convention = "ACT/ACT",
      modified = TRUE,
      comp.freq = 1
    ),
    8.911883,
    tolerance = 1e-6
  )
})
