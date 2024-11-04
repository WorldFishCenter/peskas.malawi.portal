#' Generate content for the revenue tab of the Malawi small scale fisheries dashboard
#'
#' @description
#' This function creates the layout and content for the revenue tab of the dashboard.
#' It includes a time series plot and a revenue distribution plot.
#'
#' @return A tagList containing the structured content for the revenue tab
#'
#' @export
tab_revenue_content <- function() {
  tagList(
    tags$div(
      class = "mb-4",
      page_heading(
        pretitle = "small scale fisheries",
        title = "Revenue"
      )
    ),
    district_selector("revenue-district"),
    page_cards(
      tags$div(
        class = "row g-2",
        tags$div(
          class = "col-lg-8",
          card(
            title = "Revenue Time Series",
            mod_ts_ui(id = "revenue_ts")
          )
        ),
        tags$div(
          class = "col-lg-4",
          card(
            title = "Revenue seasonal distribution",
            mod_spider_ui("revenue_spider")
          )
        )
      ),
      tags$div(
        class = "row g-2",
        tags$div(
          class = "col-12",
          card(
            title = "Revenue rates by gear",
            tooltip = "Average revenue per unit effort (MWK/hrs) by fishing gear type",
            mod_treemap_ui("revenue_treemap")
          )
        )
      )
    )
  )
}
