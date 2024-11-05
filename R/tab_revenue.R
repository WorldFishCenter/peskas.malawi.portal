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
        pretitle = "Malawi SSF",
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
            tooltip = "
    <p>This visualization shows the average revenue generated per unit of effort (RPUE) for each fishing gear type:</p>
    <ul class='mb-0'>
      <li>RPUE is calculated as revenue (MWK) per fisher per hour</li>
      <li>Size of boxes indicates relative RPUE for each gear type</li>
      <li>Colors help distinguish between different gear types</li>
      <li>Higher RPUE suggests more economically efficient fishing methods</li>
      <li>Values are averaged across all fishing trips for the selected district</li>
    </ul>
  ",
            mod_treemap_ui("revenue_treemap")
          )
        )
      )
    )
  )
}
