#
# Unit tests for the bond basis functions
#

context("test-event_study")

test_that("Event study output is as expected", {
  expect_equal(
    as.data.frame(
      event_study(
        spread_data,
        tibble::tibble(
          Series = c("A", "B"),
          EventDate = c("2019-01-29", "2019-01-29")
        )
      ) %>%
        dplyr::filter(
          Series %in% c("A", "B"),
          Date == as.POSIXct("2019-01-29 00:00:00")
        ) %>%
        dplyr::mutate(
          EventDate = as.Date(EventDate)
        )
    ),
    data.frame(
      readr::read_csv(
        "Date,Series,Value,EventDate,RebasedValue,Time
        2019-01-29,A,10.09000,2019-01-29,0,0
        2019-01-29,B,10.52935,2019-01-29,0,0"
      ),
      stringsAsFactors = FALSE
    ),
    tolerance = 1e-3
  )
})
