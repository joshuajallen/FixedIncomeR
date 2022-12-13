
plotly_std_config <- function(x) {
  # Set a NULL element ID to avoid messages of the form:
  #  Ignoring explicitly provided widget ID "6c0e221658"; Shiny doesn't use them
  x$elementId <- NULL
  plotly::config(
    x,
    displaylogo = FALSE,
    modeBarButtonsToRemove = list(
      "sendDataToCloud",
      "autoScale2d",
      "resetScale2d",
      "hoverClosestCartesian",
      "hoverCompareCartesian",
      "zoom2d",
      "pan2d",
      "select2d",
      "lasso2d",
      "zoomIn2d",
      "zoomOut2d",
      "toggleSpikelines"
    )
  )
}


plotly_std_title <- function(x, title,
                             x_coord = 0.0,
                             y_coord = 1.1,
                             font = list(size = 12)) {
  plotly::add_annotations(
    x,
    xref = "paper",
    yref = "paper",
    x = x_coord,
    y = y_coord,
    showarrow = FALSE,
    text = title,
    font = font
  )
}


plotly_std_style <- function(x) {
  plotly::style(
    x,
    hoverlabel = list(
      # bgcolor = "white",
      # font = FONT_STYLE
    )
  )
}
