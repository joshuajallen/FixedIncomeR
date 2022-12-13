#
# Unit tests for the yield curve fitting functions
#

context("test-yield_curve_fit")

test_that("Nelson-Siegel zero rate is as expected", {
  expect_equal(
    zero_coupon_nelson_siegel(1, c(0.034, 0.834, -0.941, 0.957)),
    0.2985808,
    tolerance = 1e-6
  )
})

test_that("Modelled price from curve is as expected", {
  expect_equal(
    as.data.frame(
      price_from_curve(
        c(0.034, 0.834, -0.941, 0.957),
        tibble::tibble(
          Date = as.Date(c("2027-08-15", "2028-02-15", "2028-08-15",
                           "2029-02-15", "2029-02-15")),
          Amount = c(2.5, 2.5, 2.5, 2.5, 100),
          Desc = rep("Bond 1", 5)
        ),
        zero_coupon_nelson_siegel,
        "2019-04-16"
      ),
      stringsAsFactors = FALSE
    ),
    as.data.frame(
      tibble::tibble(
        Desc = "Bond 1",
        ModelDirtyPrice = 87.39378
      ),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-6
  )
})

test_that("Squared price difference is as expected", {
  expect_equal(
    price_sq_diff(
      c(0.034, 0.834, -0.941, 0.957),
      prices = tibble::tibble(Desc = "Bond 1", DirtyPrice = 80),
      cashflows = tibble::tibble(
        Date = as.Date(c("2027-08-15", "2028-02-15", "2028-08-15",
                         "2029-02-15", "2029-02-15")),
        Amount = c(2.5, 2.5, 2.5, 2.5, 100),
        Desc = rep("Bond 1", 5)
      ),
      zero_coupon_nelson_siegel,
      "2019-04-16"
    ),
    54.67298,
    tolerance = 1e-6
  )
})
