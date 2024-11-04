#' Generate content for the catch tab
#'
#' @export
tab_catch_content <- function() {
  tagList(
    tags$div(
      class = "mb-4",
      page_heading(
        pretitle = "small scale fisheries",
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
            tooltip = "Average catch per unit effort (kg/hrs) by fishing gear type",
            mod_treemap_ui("catch_treemap")
          )
        )
      )
    )
  )
}
