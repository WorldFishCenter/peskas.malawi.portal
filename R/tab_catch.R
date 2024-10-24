#' Generate content for the catch tab of the Malawi small scale fisheries dashboard
#'
#' @description
#' This function creates the layout and content for the catch tab of the dashboard.
#' It includes a time series plot and a catch distribution plot.
#'
#' @return A tagList containing the structured content for the catch tab
#'
#' @export
tab_catch_content <- function() {
  tagList(
    page_heading(
      pretitle = "small scale fisheries",
      title = "Catch"
    ),
    page_cards(
      tags$div(
        class = "row",
        tags$div(
          class = "col-lg-8",
          card(
            title = "Catch Time Series",
            mod_catch_ts_ui("catch_ts")
          )
        ),
        tags$div(
          class = "col-lg-4",
          card(
            title = "Catch Distribution",
            mod_spider_catch_ui("spider_catch")
          )
        )
      )
    )
  )
}
