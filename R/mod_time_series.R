#' catch_ts UI Function
#' @noRd
mod_ts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    apexcharter::apexchartOutput(ns("time_series_plot"), height = "400px")
  )
}

#' catch_ts Server Functions
#'
#' @noRd
#' @importFrom dplyr %>% group_by summarise filter bind_rows mutate
#' @importFrom apexcharter apex renderApexchart ax_yaxis ax_colors ax_legend ax_tooltip
mod_ts_server <- function(id, metric_col, selected_district = NULL) {
  moduleServer(id, function(input, output, session) {
    # Static settings moved outside reactive contexts
    line_colors <- c("#AB9B96", "#D34F73")
    line_widths <- c(1, 1)
    line_dasharray <- c(0, 0)
    y_label <- paste("Median", gsub("\\.", " ", metric_col))

    # Cached data processing
    plot_data <- reactive({
      district <- selected_district()
      req(district)

      all_districts <- peskas.malawi.portal::timeseries_month %>%
        dplyr::group_by(.data$date_month) %>%
        dplyr::summarise(
          sample_district = "All districts",
          "{metric_col}" := stats::median(.data[[metric_col]], na.rm = TRUE),
          .groups = "drop"
        )

      if (district != "All districts") {
        selected <- peskas.malawi.portal::timeseries_month %>%
          dplyr::filter(.data$sample_district == district) %>%
          dplyr::select("date_month", "sample_district", dplyr::all_of(metric_col))
        dplyr::bind_rows(all_districts, selected)
      } else {
        all_districts
      }
    }) %>%
      bindCache(selected_district(), metric_col)

    # Cached mean calculation
    mean_value <- reactive({
      data <- plot_data()
      mean(data[[metric_col]], na.rm = TRUE)
    }) %>%
      bindCache(selected_district(), metric_col)

    # Plot rendering
    output$time_series_plot <- apexcharter::renderApexchart({
      data <- plot_data()

      apexcharter::apex(data,
        type = "line",
        mapping = apexcharter::aes(
          x = .data$date_month,
          y = .data[[metric_col]],
          group = .data$sample_district
        )
      ) %>%
        apexcharter::ax_chart(
          toolbar = list(show = TRUE),
          animations = list(enabled = TRUE, speed = 500)
        ) %>%
        apexcharter::ax_yaxis(
          decimalsInFloat = 2,
          title = list(text = y_label)
        ) %>%
        apexcharter::ax_xaxis(
          type = "datetime",
          title = list(text = "Date")
        ) %>%
        apexcharter::ax_tooltip(x = list(format = "MMM yyyy")) %>%
        apexcharter::ax_colors(line_colors) %>%
        apexcharter::ax_stroke(
          width = line_widths,
          dashArray = line_dasharray
        ) %>%
        apexcharter::ax_markers(size = 6) %>%
        apexcharter::ax_legend(
          position = "bottom",
          fontSize = 15
        ) %>%
        apexcharter::ax_annotations(
          yaxis = list(
            list(
              y = mean_value(),
              borderColor = "#885053",
              borderWidth = 2, # Make the line thicker
              label = list(
                text = paste("Mean", gsub("\\.", " ", metric_col)),
                style = list(
                  color = "#F1FAEE",
                  background = scales::alpha("#885053", 0.7), # Add transparency to background
                  padding = list(
                    left = 10,
                    right = 10,
                    top = 5,
                    bottom = 5
                  )
                )
              )
            )
          )
        )
    }) %>%
      bindCache(selected_district(), metric_col)
  })
}
