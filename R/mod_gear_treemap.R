#' Treemap UI Function
#' @noRd
mod_treemap_ui <- function(id) {
  ns <- NS(id)
  tagList(
    apexcharter::apexchartOutput(ns("treemap"), height = "400px")
  )
}

#' Treemap Server Function
#' @noRd
mod_treemap_server <- function(id, data, selected_district = NULL, type = "cpue") {
  moduleServer(id, function(input, output, session) {
    # Pre-compute suffix
    suffix <- if (type == "cpue") " kg/hrs" else " MWK/hrs"

    # Cache the filtered data
    plot_data <- reactive({
      district <- selected_district()
      req(district)

      dataset <- if (type == "cpue") data$cpue else data$rpue

      district_data <- dataset %>%
        dplyr::filter(
          .data$sample_district == district,
          !is.na(.data$value)
        )

      # Return the prepared series data
      list(
        list(
          data = lapply(seq_len(nrow(district_data)), function(i) {
            list(
              x = district_data$gear[i],
              y = district_data$value[i]
            )
          })
        )
      )
    }) %>% bindCache(selected_district(), type)

    # Cache the plot generation
    output$treemap <- apexcharter::renderApexchart({
      series_data <- plot_data()
      req(series_data[[1]]$data, length(series_data[[1]]$data) > 0)

      # Generate colors for unique gears
      unique_gears <- unique(series_data[[1]]$data %>%
        purrr::map_chr(~ .x$x))
      colors <- viridisLite::viridis(length(unique_gears), alpha = 0.75)

      # Create the plot
      apexcharter::apexchart() %>%
        apexcharter::ax_chart(
          type = "treemap",
          toolbar = list(show = TRUE),
          animations = list(
            enabled = TRUE,
            speed = 500, # Slightly reduced for better performance
            animateGradually = list(enabled = TRUE)
          )
        ) %>%
        apexcharter::ax_series2(series_data) %>%
        apexcharter::ax_colors(colors) %>%
        apexcharter::ax_plotOptions(
          treemap = list(
            distributed = TRUE,
            enableShades = FALSE
          )
        ) %>%
        apexcharter::ax_dataLabels(
          enabled = TRUE,
          formatter = htmlwidgets::JS(sprintf(
            "function (text, op) {return [text, op.value.toFixed(2) + '%s']}",
            suffix
          ))
        )
    }) %>% bindCache(plot_data(), suffix)
  })
}
