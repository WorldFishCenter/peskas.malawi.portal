#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # HOME TAB
  mod_district_summary_table_server("district_table", peskas.malawi.portal::table_data)
  mod_hex_map_server(id = "hex_map")

  # CATCH TAB
  mod_ts_server(
    id = "catch_ts",
    metric_col = "Catch (kg)",
    selected_district = reactive(input$district)
  )

  mod_spider_server(
    id = "catch_spider",
    metric_col = "Catch (kg)",
    data = peskas.malawi.portal::spider_data,
    selected_district = reactive(input$district)
  )

  mod_treemap_server(
    id = "catch_treemap",
    data = peskas.malawi.portal::treeplot_data,
    selected_district = reactive(input$district),
    type = "cpue"
  )

  # REVENUE TAB
  mod_ts_server(
    id = "revenue_ts",
    metric_col = "Catch Value (MWK)",
    selected_district = reactive(input$district)
  )

  mod_spider_server(
    id = "revenue_spider",
    metric_col = "Catch Value (MWK)",
    data = peskas.malawi.portal::spider_data,
    selected_district = reactive(input$district)
  )

  mod_treemap_server(
    id = "revenue_treemap",
    data = peskas.malawi.portal::treeplot_data,
    selected_district = reactive(input$district),
    type = "rpue"
  )

  mod_validation_server("validation")
}
