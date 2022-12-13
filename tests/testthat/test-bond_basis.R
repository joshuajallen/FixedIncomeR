#
# Unit tests for the bond basis functions
#

context("test-bond_basis")

test_that("Correlated series generation produces expected outputs", {
  expect_equal(
    as.data.frame(
      generate_correlated_series(
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
        start_date = "2019-04-16",
        field_descs = "Yield",
        seed = 0
      )[c(200, 400, 600, 800),]
    ),
    data.frame(
      Date = c(as.Date("2020-01-18"), as.Date("2019-10-26"),
               as.Date("2019-08-03"), as.Date("2019-05-11")),
      Desc = c("Bond A", "Bond 2", "Bond 3", "Bond X"),
      Field = rep("Yield", 4),
      Value = c(0.02700115, 0.02794946, 0.02638271, 0.02751733),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-6
  )
})

test_that("Bond pricing from yield works as expected", {
  expect_equal(
    price_treasury_bond(
      "2029-06-30",
      0.02,
      "2025-06-30",
      yield = 0.018
    )$dirtyPrice,
    100.6982,
    tolerance = 0.01
  )
})

test_that("Bond pricing from price works as expected", {
  expect_equal(
    price_treasury_bond(
      "2029-06-30",
      0.02,
      "2025-06-30",
      price = 102.00
    )$yield,
    0.01467111,
    tolerance = 0.01
  )
})

test_that("Levels can be converted to level and spreads", {
  expect_equal(
    to_level_and_spreads(tibble::tibble(
      Date = c(1, 2, 3, 1, 2, 3),
      Desc = c(rep("Bond A", 3), rep("Bond B", 3)),
      Field = rep("Yield", 6),
      Value = c(2, 2, 2, 2.5, 2.5, 2.5)
    )),
    tibble::tibble(
      Date = c(1, 2, 3, 1, 2, 3),
      Desc = c(rep("Bond A", 3), rep("Bond B", 3)),
      Field = c(rep("Yield", 3), rep("Spread", 3)),
      Value = c(2, 2, 2, 0.5, 0.5, 0.5)
    )
  )
})
