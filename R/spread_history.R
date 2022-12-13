#' Generate a spread history box plot
#'
#' @param df A data frame of time series, with columns Date, Series, Value. A Labels column
#'           is optional
#' @param n_points Max number of historic points to include.
#' @param y_label A label for the field to be plotted.
#' @param source_arg The source label to be passed to the plot_ly object.
#' @export
#' @examples
#' library(FIRVr)
#' plot_spread_history(spread_data, n_points = 63, y_label = "Spread")
#' @importFrom magrittr "%>%"
plot_spread_history <- function(df,
                                n_points = 63,
                                y_label = "Spread",
                                source_arg = "plot_spread_history_plot",
                                height = 400) {

  stopifnot(is.data.frame(df))
  stopifnot(nrow(df) > 3)

  first_date <- df$Date %>%
    unique() %>%
    sort() %>%
    tail(n_points) %>%
    head(1)

  most_recent <- max(df$Date, na.rm = TRUE)

  df <- df %>%
    dplyr::group_by(Series) %>%
    dplyr::mutate(
      LatestPoint = if_else(Date == max(Date), TRUE, FALSE),
      Stale = if_else(max(Date) == most_recent, FALSE, TRUE),
      Symbol = if_else(Stale, 4, 24) # X for stale data
    )

  current_data <- df %>%
    dplyr::filter(LatestPoint)

  historic_data <- df %>%
    dplyr::filter(!LatestPoint & Date >= first_date)

  p <- plotly::plot_ly(
    height = height,
    source = source_arg
  ) %>%
    plotly::add_trace(
      name = paste0("Historic (", n_points, "D)"),
      data = historic_data,
      x = ~Series,
      y = ~Value,
      type = "box",
      boxpoints = "outliers",
      boxmean = TRUE,
      fillcolor = "lightsteelblue",
      marker = list(color = "grey", size = 3),
      line = list(color = "grey", size = 2)
    ) %>%
    plotly::add_trace(
      name = "Current",
      data = current_data,
      x = ~Series,
      y = ~Value,
      type = "scatter",
      mode = "markers",
      marker = list(size = 7, color = "darkred", symbol = ~Symbol)
    )  %>%
    plotly_std_config() %>%
    plotly_std_style %>%
    plotly_std_title(y_label, y_coord = 1.07) %>%
    plotly::layout(
      xaxis = list(
        "title" = "",
        "tickangle" = 270,
        "showticklabels" = FALSE
      ),
      yaxis = list(
        "title" = ""
      ),
      showlegend = FALSE
    )

  #
  # Add labels if they were provided
  #

  if ( "Labels" %in% names(df) ) {

    # Calculate limits for label positioning (over, then under)
    limit_data <- df %>%
      dplyr::filter(Date >= first_date) %>%
      dplyr::group_by(Series) %>%
      dplyr::summarise(
        Min = min(Value, na.rm = TRUE),
        Max = max(Value, na.rm = TRUE)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        LabelY = if_else(dplyr::row_number() %% 2 == 0, Min, Max),
        TextPosition = if_else(dplyr::row_number() %% 2 == 0, "top", "bottom")
      ) %>%
      dplyr::left_join(current_data %>% dplyr::select(Series, Labels), by = "Series")


    p <- p %>%
      plotly::add_annotations(
        data = limit_data,
        text = ~Labels,
        x = ~Series,
        y = ~LabelY,
        xref = "x",
        yref = "y",
        yanchor = ~TextPosition,
        showarrow = FALSE,
        font = list(size = 8)
      )

  }

  p

}
