#' catch_ts UI Function
#'
#' @description A shiny Module for displaying catch time series, integrated with Tabler layout.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList selectInput
#' @importFrom apexcharter apexchartOutput
mod_ts_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "row",
      tags$div(
        class = "col-md-4 col-lg-3", # Adjust these classes to control the width
        tags$div(
          class = "form-group",
          tags$label(class = "form-label", `for` = ns("district"), "Select District"),
          tags$select(
            id = ns("district"),
            class = "form-select",
            tags$option(value = "All districts", "All districts"),
            lapply(
              setdiff(unique(peskas.malawi.portal::timeseries_month$sample_district), "All districts"),
              function(x) tags$option(value = x, x)
            )
          )
        )
      )
    ),
    apexcharter::apexchartOutput(ns("time_series_plot"), height = "400px")
  )
}

#' catch_ts Server Functions
#'
#' @noRd
#' @importFrom dplyr %>% group_by summarise filter bind_rows mutate
#' @importFrom apexcharter apex renderApexchart ax_yaxis ax_colors ax_legend ax_tooltip
mod_ts_server <- function(id, metric_col) {
  moduleServer(id, function(input, output, session) {
    # Static settings moved outside reactive contexts
    line_colors <- c("#AB9B96", "#D34F73")
    line_widths <- c(1, 1)
    line_dasharray <- c(0, 0)
    y_label <- paste("Median", gsub("\\.", " ", metric_col))

    # Cached data processing
    plot_data <- reactive({
      req(input$district)

      all_districts <- peskas.malawi.portal::timeseries_month %>%
        dplyr::group_by(.data$date_month) %>%
        dplyr::summarise(
          sample_district = "All districts",
          "{metric_col}" := stats::median(.data[[metric_col]], na.rm = TRUE),
          .groups = "drop"
        )

      if (input$district != "All districts") {
        selected <- peskas.malawi.portal::timeseries_month %>%
          dplyr::filter(.data$sample_district == input$district) %>%
          dplyr::select("date_month", "sample_district", dplyr::all_of(metric_col))
        dplyr::bind_rows(all_districts, selected)
      } else {
        all_districts
      }
    }) %>%
      bindCache(input$district, metric_col) # Cache based on both inputs

    # Cached mean calculation
    mean_value <- reactive({
      data <- plot_data()
      mean(data[[metric_col]], na.rm = TRUE)
    }) %>%
      bindCache(input$district, metric_col)

    # Cached plot rendering
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
        apexcharter::ax_theme(mode = "light") %>%
        apexcharter::ax_annotations(
          yaxis = list(
            list(
              y = mean_value(),
              borderColor = "#DB7F67",
              label = list(
                text = paste("Mean", gsub("\\.", " ", metric_col)),
                style = list(
                  color = "#000000",
                  background = "#DB7F67"
                )
              )
            )
          )
        )
    }) %>%
      bindCache(input$district, metric_col)
  })
}
## To be copied in the UI
# mod_catch_ts_ui("catch_ts_1")
## To be copied in the server
# mod_catch_ts_server("catch_ts_1")
