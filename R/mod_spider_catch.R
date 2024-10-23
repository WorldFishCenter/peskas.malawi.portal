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
        data = peskas.malawi.portal::spider_catch_data, type = "radar",
        mapping = apexcharter::aes(x = .data$month, y = .data$catch_kg, group = .data$sample_district)
      ) %>%
        apexcharter::ax_legend(
          position = "bottom",
          fontSize = 15
        )
    })
  })
}
