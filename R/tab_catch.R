tab_catch_content <- function() {
  tagList(
    page_heading(pretitle = "small scale fisheries", title = "Catch"),
    page_cards(
      tags$div(
        class = "row", # Add a row container
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
