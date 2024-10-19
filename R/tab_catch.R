tab_catch_content <- function() {
  tagList(
    page_heading(pretitle = "small scale fisheries", title = "Catch"),
    page_cards(
      tags$div(
        class = "col-lg-8",
        card(
          title = "Catch Time Series",
          mod_catch_ts_ui("catch_ts")
        )
      )
    )
  )
}
