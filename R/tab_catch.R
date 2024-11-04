#' Generate content for the catch tab
#'
#' @export
tab_catch_content <- function() {
  tagList(
    page_heading(
      pretitle = "small scale fisheries",
      title = "Catch"
    ),
    district_selector("catch-district"),
    page_cards(
      tags$div(
        class = "row",
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
      )
    )
  )
}
