#' Generate a plot of pca residuals
#'
#' @param df A data frame of time series, with columns Date, Series, Value. A Labels column
#'           is optional
#' @param n_points The number of historic points to include.
#' @param roll_points The number of historich point to include in the rolling window.
#' @param y_label A label for the field to be plotted.
#' @param show_z_score Centre and scale before performing PCA.
#' @param source_arg The source label to be passed to the plot_ly object.
#' @export
#' @examples
#' library(FIRVr)
#' plot_pca_residuals(spread_data, n_points = 63, y_label = "Residual")
#' @importFrom magrittr "%>%"
plot_pca_residuals <- function(df,
                               n_points = 126,
                               y_label = "Residual",
                               show_z_score = FALSE,
                               source_arg = "pca_residuals_plot",
                               height = 400) {

  stopifnot(is.data.frame(df))
  stopifnot(nrow(df) > 3)

  first_date <- df$Date %>%
    unique() %>%
    sort() %>%
    tail(n_points) %>%
    head(1)

  df <- df %>%
    dplyr::filter(Date >= first_date)

  roll_window <- as.integer(n_points / 2)

  tickers_by_maturity <- df %>%
    dplyr::filter(Date == max(Date)) %>%
    dplyr::arrange(MATURITY) %>%
    dplyr::pull(TICKER)

  wide_data <- df %>%
    dplyr::select(Date, TICKER, Value) %>%
    tidyr::spread(key = "TICKER", value = "Value") %>%
    dplyr::select(-Date) %>%
    dplyr::select(tickers_by_maturity) %>%
    dplyr::select_if(~sum(is.na(.)) < 5) %>% # Remove columns with many NAs
    tidyr::drop_na() %>% # Remove rows that have remaining NAs
    as.matrix()

  get_current_residual <- function(data_matrix) {
    pca <- prcomp(data_matrix, center = show_z_score, scale = show_z_score)
    residuals <- pca$x %*% t(pca$rotation) - pca$x[, 1:3] %*% t(pca$rotation[, 1:3])
    residuals %>% tail(1) %>% as.numeric()
  }

  rolling_residuals <- zoo::rollapply(
    wide_data,
    roll_window,
    get_current_residual,
    by.column = FALSE
  )

  colnames(rolling_residuals) <- colnames(wide_data)

  # Add back series for which we could not calculate residuals (helpful
  # for alignment of plots)
  complete_residuals <- matrix(
    data = NA,
    ncol = length(tickers_by_maturity),
    nrow = nrow(rolling_residuals),
    dimnames = list(NULL, tickers_by_maturity)
  )
  complete_residuals[, colnames(rolling_residuals)] <- rolling_residuals

  plot_data <- tibble::tibble(
    Ticker = factor(colnames(complete_residuals),
                    levels = colnames(complete_residuals) %>% unique()),
    Current = tail(complete_residuals, 1) %>% as.numeric(),
    Average = colMeans(head(complete_residuals, -1)) %>% as.numeric()
  )

  p <- plotly::plot_ly(data = plot_data) %>%
    plotly::add_trace(
      name = "Current",
      x = ~Ticker,
      y = ~Current,
      type = "scatter",
      mode = "markers",
      marker = list(size = 7, color = "darkred", symbol = 24)
    ) %>%
    plotly::add_trace(
      name = "Recent Average",
      x = ~Ticker,
      y = ~Average,
      type = "bar",
      marker = list(color = "lightsteelblue", size = 3)
    ) %>%
    plotly_std_config() %>%
    plotly_std_style %>%
    plotly_std_title(y_label, y_coord = 1.07) %>%
    plotly_std_title(
      paste0("Rolling window is half of history period"),
      x_coord = 0.95,
      y_coord = 1.07
    ) %>%
    plotly::layout(
      xaxis = list(
        "title" = "",
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

    # Make sure series are aligned (just in case we decide to omit NA series above)
    aligned_labels <- plot_data %>%
      dplyr::mutate(TICKER = as.character(Ticker)) %>% # Convert from factor
      dplyr::left_join(df %>% dplyr::filter(Date == max(Date)), by = "TICKER") %>%
      dplyr::select(Ticker, Current, Average, Labels) %>%
      dplyr::mutate(
        TextPosition = dplyr::if_else(dplyr::row_number() %% 2 == 0, "top", "bottom")
      ) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(
        Min = if_else(is.na(Current), 0, min(Current, Average, 0)),
        Max = if_else(is.na(Current), 0, max(Current, Average, 0))
      ) %>%
      ungroup() %>%
      mutate(
        LabelY = if_else(dplyr::row_number() %% 2 == 0, Min, Max)
      )

    p <- p %>%
      plotly::add_annotations(
        data = aligned_labels,
        text = ~Labels,
        x = ~Ticker,
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
