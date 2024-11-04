#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # HOME TAB
  mod_district_summary_table_server("district_table", peskas.malawi.portal::table_data)

  # CATCH TAB
  mod_ts_server(
    id = "catch_ts",
    metric_col = "Catch (kg)",
    selected_district = reactive(input$`catch-district`)
  )

  mod_spider_server(
    id = "catch_spider",
    metric_col = "Catch (kg)",
    data = peskas.malawi.portal::spider_data,
    selected_district = reactive(input$`catch-district`)
  )

  mod_treemap_server(
    id = "catch_treemap",
    data = peskas.malawi.portal::treeplot_data,
    selected_district = reactive(input$`catch-district`),
    type = "cpue"
  )

  # REVENUE TAB
  mod_ts_server(
    id = "revenue_ts",
    metric_col = "Catch Value (MWK)",
    selected_district = reactive(input$`revenue-district`)
  )

  mod_spider_server(
    id = "revenue_spider",
    metric_col = "Catch Value (MWK)",
    data = peskas.malawi.portal::spider_data,
    selected_district = reactive(input$`revenue-district`)
  )

  mod_treemap_server(
    id = "revenue_treemap",
    data = peskas.malawi.portal::treeplot_data,
    selected_district = reactive(input$`revenue-district`),
    type = "rpue"
  )
}
