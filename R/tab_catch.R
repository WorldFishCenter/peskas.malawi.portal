#' Generate content for the catch tab
#'
#' @export
tab_catch_content <- function() {
  tagList(
    tags$div(
      class = "mb-4",
      page_heading(
        pretitle = "Malawi SSF",
        title = "Catch"
      )
    ),
    district_selector("catch-district"),
    page_cards(
      # First row of cards - Time series and Spider plot
      tags$div(
        class = "row g-2",
        tags$div(
          class = "col-lg-8",
          card(
            title = "Catch Time Series",
            mod_ts_ui(id = "catch_ts")
          )
        ),
        tags$div(
          class = "col-lg-4",
          card(
            title = "Catch seasonal distribution",
            mod_spider_ui("catch_spider")
          )
        )
      ),
      # Second row - Treemap
      tags$div(
        class = "row g-2",
        tags$div(
          class = "col-12",
          card(
            title = "Catch rates by gear",
            tooltip = "
    <p>This visualization shows the average catch per unit of effort (CPUE) for each fishing gear type:</p>
    <ul class='mb-0'>
      <li>CPUE is calculated as catch (kg) per fisher per hour</li>
      <li>Size of boxes indicates relative CPUE for each gear type</li>
      <li>Colors help distinguish between different gear types</li>
      <li>Higher CPUE suggests more efficient fishing methods</li>
      <li>Values are averaged across all fishing trips for the selected district</li>
    </ul>
  ",
            mod_treemap_ui("catch_treemap")
          )
        )
      )
    )
  )
}
