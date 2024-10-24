#' spider_catch UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_spider_catch_ui <- function(id) {
  ns <- NS(id)
  tagList(
    apexcharter::apexchartOutput(ns("spider_plot"), height = "468px")
  )
}

#' spider_catch Server Functions
#'
#' @noRd
mod_spider_catch_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$spider_plot <- apexcharter::renderApexchart({
      apexcharter::apex(
        data = peskas.malawi.portal::spider_catch_data, ,
        type = "radar",
        mapping = apexcharter::aes(
          x = .data$month,
          y = .data$`Catch (kg)`,
          group = .data$District
        )
      ) %>%
        apexcharter::ax_legend(
          position = "bottom",
          fontSize = 15
        ) %>%
        apexcharter::ax_chart(
          toolbar = list(show = TRUE)
        ) %>%
        apexcharter::ax_tooltip(
          y = list(
            formatter = htmlwidgets::JS("function(val) { return val.toFixed(2) + ' kg'; }")
          )
        ) %>%
        apexcharter::ax_chart(
          toolbar = list(show = TRUE),
          animations = list(
            enabled = TRUE, # Enable animations
            easing = "easeinout", # Control the easing function (can be 'linear', 'easein', etc.)
            speed = 500 # Speed up the animation (in milliseconds)
          )
        ) %>%
        apexcharter::ax_colors(c("#9d4edd", "#8d99ae", "#e07a5f", "#3d405b", "#81b29a", "#f2cc8f", "#0a9396"))
    })
  })
}
