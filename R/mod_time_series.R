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
mod_catch_ts_ui <- function(id) {
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
mod_catch_ts_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    plot_data <- reactive({
      req(input$district)
      all_districts <- peskas.malawi.portal::timeseries_month %>%
        dplyr::group_by(.data$date_month) %>%
        dplyr::summarise(
          sample_district = "All districts",
          catch_kg = stats::median(.data$catch_kg, na.rm = TRUE)
        )
      if (input$district != "All districts") {
        selected <- peskas.malawi.portal::timeseries_month %>%
          dplyr::filter(.data$sample_district == input$district)
        dplyr::bind_rows(all_districts, selected)
      } else {
        all_districts
      }
    })

    output$time_series_plot <- apexcharter::renderApexchart({
      data <- plot_data()

      # Define colors, widths, and types for each line if you have multiple sample districts
      line_colors <- c("#AB9B96", "#D34F73") # Example colors
      line_widths <- c(1, 1) # Example widths for each line
      line_dasharray <- c(0, 0) # Dash patterns (0: solid, 5: dashed, etc.)

      # First, calculate the mean catch_kg
      mean_catch <- mean(data$catch_kg, na.rm = TRUE)

      # Then, modify the plot to include the horizontal line
      apexcharter::apex(data,
        type = "line",
        mapping = apexcharter::aes(x = .data$date_month, y = .data$catch_kg, group = .data$sample_district)
      ) %>%
        apexcharter::ax_chart(toolbar = list(show = TRUE)) %>%
        apexcharter::ax_yaxis(decimalsInFloat = 2, title = list(text = "Catch (kg)")) %>%
        apexcharter::ax_xaxis(type = "datetime", title = list(text = "Date")) %>%
        apexcharter::ax_legend(position = "top") %>%
        apexcharter::ax_tooltip(x = list(format = "MMM yyyy")) %>%
        apexcharter::ax_colors(line_colors) %>% # Assign custom colors
        apexcharter::ax_stroke(
          width = line_widths, # Set widths for each line
          dashArray = line_dasharray # Set dash pattern for each line
        ) %>%
        apexcharter::ax_markers(size = 6) %>%
        apexcharter::ax_legend(
          position = "bottom",
          fontSize = 15
        ) %>%
        apexcharter::ax_theme(mode = "light") %>%
        # Add a horizontal line representing the mean
        apexcharter::ax_annotations(
          yaxis = list(
            list(
              y = mean_catch, # Position the line at the mean value
              borderColor = "#DB7F67", # Color of the line
              label = list(
                text = "Mean Catch", # Label for the line
                style = list(
                  color = "#000000", # Color of the label text
                  background = "#DB7F67" # Background color of the label
                )
              )
            )
          )
        )
    })
  })
}

## To be copied in the UI
# mod_catch_ts_ui("catch_ts_1")
## To be copied in the server
# mod_catch_ts_server("catch_ts_1")
