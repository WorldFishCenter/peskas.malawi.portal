#' spider UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spider_ui <- function(id) {
  ns <- NS(id)
  tagList(
    apexcharter::apexchartOutput(ns("spider_plot"), height = "468px")
  )
}

#' spider Server Functions
#'
#' @noRd
mod_spider_server <- function(id, metric_col, data = peskas.malawi.portal::spider_data, col_palette = NULL) {
  # Pre-compute metric symbol outside the server
  metric_sym <- rlang::sym(metric_col)

  moduleServer(id, function(input, output, session) {
    output$spider_plot <- apexcharter::renderApexchart({
      # Single unit extraction
      unit <- if (grepl("\\((.*)\\)", metric_col)) {
        gsub(".*\\((.*)\\).*", "\\1", metric_col)
      }

      plot <- apexcharter::apex(
        data = data,
        type = "radar",
        mapping = apexcharter::aes(
          x = .data$month,
          y = !!metric_sym,
          group = .data$District
        )
      ) %>%
        apexcharter::ax_legend(position = "bottom") %>%
        apexcharter::ax_chart(
          animations = list(enabled = TRUE, speed = 500)
        ) %>%
        apexcharter::ax_tooltip(
          y = list(
            formatter = htmlwidgets::JS(sprintf(
              "function(val) { return val.toFixed(2)%s }",
              if (!is.null(unit)) paste0(" + ' ", unit, "'") else ""
            ))
          )
        )

      if (!is.null(col_palette)) {
        plot <- plot %>% apexcharter::ax_colors(col_palette)
      }

      plot
    }) %>%
      bindCache(metric_col, data, col_palette) # Cache based on inputs that would change the plot
  })
}
