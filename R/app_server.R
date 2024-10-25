#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  districts_colors = c("#9d4edd", "#8d99ae", "#e07a5f", "#3d405b","#81b29a", "#f2cc8f", "#0a9396")
  # CATCH TAB
  mod_ts_server(id = "catch_ts", metric_col = "Catch (kg)")
  mod_spider_server(id = "catch_spider", metric_col = "Catch (kg)", col_palette = districts_colors)

  # REVENUE TAB
  mod_ts_server(id = "revenue_ts", metric_col = "Catch Value (MWK)")
  mod_spider_server(id = "revenue_spider", metric_col = "Catch Value (MWK)", col_palette = districts_colors)
}
