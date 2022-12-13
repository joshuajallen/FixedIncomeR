#
# Unit tests for the yield curve fitting functions
#

context("test-bond_maths")

# test_that("Zero bond curve is fitted correctly", {
#   expect_equal(
#     as.data.frame(
#       fit_bond_zero_curve(
#         c(5, 10, 30),
#         c(0.0, 0.0, 0.0),
#         c(103.017, 104.024, 100.332),
#         "Annual",
#         2,
#         "ActualActual.Euro",
#         "ModifiedFollowing",
#         curve_method = "ExponentialSplinesFitting",
#         start_date = "2020-04-23"
#       ),
#       stringsAsFactors = FALSE
#     )[c(100, 1000, 5000), "ZeroRate"],
#     c(-0.003391242, -0.003370253, -0.003307973),
#     tolerance = 1e-6
#   )
# })
