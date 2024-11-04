#' spider UI Function
#'
#' @description A shiny Module that shares input with time series.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spider_ui <- function(id) {
  ns <- NS(id)
  tagList(
    apexcharter::apexchartOutput(ns("spider_plot"))
  )
}

#' spider Server Functions
#'
#' @noRd
mod_spider_server <- function(id, metric_col, data = peskas.malawi.portal::spider_data,
                              selected_district = NULL, pre_avg = NULL) {
  moduleServer(id, function(input, output, session) {
    # Process data based on district selection
    plot_data <- reactive({
      district <- selected_district()
      req(district)

      all_districts_avg <- if (!is.null(pre_avg)) {
        pre_avg
      } else {
        data %>%
          dplyr::group_by(.data$month) %>%
          dplyr::summarise(
            "{metric_col}" := mean(.data[[metric_col]], na.rm = TRUE),
            .groups = "drop"
          ) %>%
          dplyr::mutate(District = "All districts")
      }

      if (district != "All districts") {
        selected_district_data <- data %>%
          dplyr::filter(.data$District == district)

        dplyr::bind_rows(all_districts_avg, selected_district_data)
      } else {
        all_districts_avg
      }
    }) %>%
      bindCache(selected_district(), metric_col)

    # Render the plot
    output$spider_plot <- apexcharter::renderApexchart({
      plot_data <- req(plot_data())
      district <- req(selected_district())

      apexcharter::apex(
        data = plot_data,
        type = "radar",
        mapping = apexcharter::aes(
          x = .data$month,
          y = !!rlang::sym(metric_col),
          group = .data$District
        )
      ) %>%
        apexcharter::ax_chart(
          toolbar = list(show = TRUE),
          animations = list(enabled = TRUE, speed = 500)
        ) %>%
        apexcharter::ax_colors(c("#AB9B96", "#D34F73")) %>%
        apexcharter::ax_legend(position = "bottom") %>%
        apexcharter::ax_tooltip(
          enabled = TRUE,
          y = list(
            formatter = htmlwidgets::JS("function(val) { return val.toFixed(2); }")
          )
        )
    }) %>%
      bindCache(selected_district(), metric_col)
  })
}
