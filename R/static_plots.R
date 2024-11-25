#' Create a homecard time series plot
#'
#' @param data A dataframe containing the time series data
#' @param x_col The name of the column to be used for the x-axis (default: "date_month")
#' @param y_col The name of the column to be used for the y-axis
#' @param type The type of plot (default: "area")
#'
#' @return An apexchart object
#'
homecard_ts_plot <- function(data, x_col = "date_month", y_col, type = "area") {
  # Pre-compute the symbols
  y_sym <- rlang::sym(y_col)
  x_sym <- rlang::sym(x_col)

  # Extract unique years from the date column
  unique_years <- unique(format(data[[x_col]], "%Y"))
  year_annotations <- lapply(unique_years, function(year) {
    year_start <- as.POSIXct(paste0(year, "-01-01"), tz = "UTC")
    year_start_ts <- as.numeric(year_start) * 1000 # Convert to JS timestamp

    list(
      x = year_start_ts,
      strokeDashArray = 0, # Solid line
      borderColor = "#CCCCCC", # Line color
      label = list(
        text = year,
        offsetX = 20, # Move label slightly to the right
        offsetY = -10, # Adjust vertical placement
        style = list(
          color = "#41b6c4", # Match the line color
          background = "transparent", # Fully transparent background
          fontSize = "12px",
          fontWeight = "bold", # Make the text bold for clarity
          padding = 0 # Remove unnecessary padding
        )
      )
    )
  })

  # Create the chart
  data %>%
    apexcharter::apex(
      apexcharter::aes(x = !!x_sym, y = !!y_sym),
      type = type
    ) %>%
    apexcharter::ax_chart(
      toolbar = list(show = FALSE),
      sparkline = list(enabled = TRUE),
      animations = list(enabled = FALSE)
    ) %>%
    apexcharter::ax_xaxis(
      labels = list(show = FALSE),
      axisTicks = list(show = FALSE),
      axisBorder = list(show = FALSE)
    ) %>%
    apexcharter::ax_yaxis(
      labels = list(show = FALSE),
      axisTicks = list(show = FALSE),
      axisBorder = list(show = FALSE)
    ) %>%
    apexcharter::ax_grid(show = FALSE) %>%
    apexcharter::ax_colors("#41b6c4") %>%
    apexcharter::ax_stroke(width = 1) %>%
    apexcharter::ax_annotations(
      xaxis = year_annotations # Add vertical line annotations
    )
}

#' Memoised version of homecard_ts_plot
#'
#' @inheritParams homecard_ts_plot
#' @export
homecard_ts_plot_memo <- memoise::memoise(homecard_ts_plot)
