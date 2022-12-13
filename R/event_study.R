#' Create rebased event study data from timeseries and event dates.
#'
#' This method could be used for example to inspect the development
#' of spreads around historical bond auction dates.
#'
#' @param df Data frame of time series with columns Date, Series and Value.
#' @param event_dates Tibble of event dates with columns Series and EventDate.
#' @return A data frame of time series, rebased around the event dates, with
#'         columns Date, Series, Value, EventDate, Time and RebasedValue
#' @export
#' @examples
#' library(FIRVr)
#' library(readr)
#' event_dates <- readr::read_csv(
#' "Series,EventDate
#' A,2018-09-07
#' A,2018-08-24
#' B,2018-10-08
#' B,2018-09-10
#' B,2018-09-27
#' B,2018-12-10"
#' )
#' event_study(spread_data, event_dates)
#' @importFrom magrittr "%>%"
event_study <- function(df, event_dates) {

  # Convert dates to datetimes so that we can manage intraday data if need be
  df_dt <- df %>%
    dplyr::mutate(Date = as.POSIXct(Date))

  event_dates_dt <- event_dates %>%
    dplyr::mutate(EventDate = as.POSIXct(EventDate))

  # Identify number of seconds in our trading day for the case of intraday data.
  # Will evaluate to zero for daily or less frequent data. In this case we will
  # set a daily scaling factor below.
  seconds_per_day <- df_dt %>%
    dplyr::mutate(Day = as.Date(Date)) %>%
    dplyr::group_by(Day) %>%
    dplyr::summarise(TradingDay = difftime(max(Date), min(Date), units = "s")) %>%
    dplyr::pull(TradingDay) %>%
    as.numeric() %>%
    max()

  # Join against event dates
  df_with_events <- df_dt %>%
    dplyr::left_join(event_dates_dt, by = "Series") %>%
    dplyr::group_by(Series, EventDate) %>%
    dplyr::arrange(Date) %>%
    dplyr::mutate(
      RowNumber = dplyr::row_number(),
      TimeToEvent = difftime(Date, EventDate, units = "s") %>% round() %>% as.integer(),
      TimeIntervalSecs = difftime(Date, lag(Date), units = "s") %>% round() %>% as.integer(),
      # Get rid of overnight intervals/day edge NAs by setting all rows to the min value
      TimeIntervalSecs = min(TimeIntervalSecs, na.rm = TRUE)
    )

  # Rebase levels to the event dates and set trading time offsets from event
  df_with_events %>%
    dplyr::group_by(EventDate, Series) %>%
    dplyr::left_join(
      df_with_events %>%
        dplyr::filter(TimeToEvent == 0) %>%
        dplyr::select(Value, EventDate, Series, RowNumber, TimeIntervalSecs),
      by = c("EventDate", "Series"),
      suffix = c("", "_Base")
    ) %>%
    dplyr::mutate(
      RebasedValue = Value - Value_Base,
      RebasedRowNumber = (RowNumber - RowNumber_Base),
      Time = RebasedRowNumber *
        TimeIntervalSecs / dplyr::if_else(seconds_per_day == 0, 60*60*24, seconds_per_day)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      EventDate = factor(EventDate)
    ) %>%
    dplyr::select(
      -Value_Base,
      -RowNumber, -RowNumber_Base, -RebasedRowNumber,
      -TimeToEvent,
      -TimeIntervalSecs, -TimeIntervalSecs_Base
    )

}


#' Plot an event study data frame that was generated with FIRVr::event_study()
#'
#' @param df A tibble of time series, rebased around the event dates, with
#'           columns Date, Series, Value, EventDate, Time and RebasedValue.
#' @param series Which series to use for the plot.
#' @param lookback Number of time intervals to look back from the event.
#' @param show_mean Show the mean of the series for all events
#' @param show_median Show the median of the series for all events
#' @param lookforward Number of time intervals to look forward from the event.
#' @export
#' @examples
#' library(FIRVr)
#' event_dates <- readr::read_csv(
#'   "Series,EventDate
#'   A,2018-09-07
#'   A,2018-08-24
#'   B,2018-10-08
#'   B,2018-09-10
#'   B,2018-09-27
#'   B,2018-12-10"
#' )
#' plot_event_study(event_study(spread_data, event_dates), series = "A")
#' @seealso \code{\link{event_study}}
#' @importFrom magrittr "%>%"
plot_event_study <- function(df,
                             series = df$Series[1],
                             lookback = 10,
                             lookforward = 10,
                             show_mean = FALSE,
                             show_median = FALSE,
                             show_plot = TRUE) {

  plot_data <- df %>%
    dplyr::filter(
      Series == series,
      Time >= -lookback,
      Time <= lookforward
    )

  p <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = plot_data,
      ggplot2::aes(
        x = Time,
        y = RebasedValue,
        group = EventDate,
        color = EventDate
      )
    ) +
    ggplot2::labs(
      x = "Trading Days",
      y = "",
      title = paste0("Rebased values for ", series, " around events")
    ) +
    ggplot2::theme_minimal()

  if ( show_mean ) {
    mean_df <- plot_data %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(Mean = mean(RebasedValue, na.rm = TRUE))
    p <- p +
      ggplot2::geom_line(
        data = mean_df,
        ggplot2::aes(x = Time, y = Mean),
        color = "black",
        size = 1
      )
  }

  if ( show_median ) {
    median_df <- plot_data %>%
      dplyr::group_by(Time) %>%
      dplyr::summarise(Median = median(RebasedValue, na.rm = TRUE))
    p <- p +
      ggplot2::geom_line(
        data = median_df,
        ggplot2::aes(x = Time, y = Median),
        color = "black",
        linetype = "dashed",
        size = 1
      )
  }

  if (show_plot) {
    plot(p)
  }

  p

}
