page_cards <- function(...) {
  tags$div(
    class = "page-body",
    tags$div(
      class = "container-xl",
      tags$div(
        class = "row row-deck row-cards",
        ...
      )
    )
  )
}

card <- function(title = "Card title", ...) {
  tags$div(
    class = "card",
    tags$div(
      class = "card-body",
      tags$h3(
        class = "card-title",
        title
      ),
      ...
    )
  )
}


#' Create a small card with an embedded plot that fills the entire card and ensures visible tooltip
#'
#' @param title The title of the card
#' @param plot The apexchart object to embed
#' @param card_height The height of the card (e.g., "200px", "15rem")
#'
#' @return A shiny tag
small_card <- function(title = "Card title", plot) {
  tagList(
    tags$head(
      tags$style(HTML("
        .card-plot-container .apexcharts-tooltip {
          overflow: visible !important;
          z-index: 1000 !important;
        }
      "))
    ),
    tags$div(
      class = "col-sm-6 col-lg-4",
      tags$div(
        class = "card",
        style = sprintf("height: %s;", "120px"),
        tags$div(
          class = "card-body p-0 d-flex flex-column h-100",
          tags$h3(
            class = "card-title m-2",
            title
          ),
          tags$div(
            class = "flex-grow-1 card-plot-container",
            style = "min-height: 0; position: relative;",
            plot
          )
        )
      )
    )
  )
}


# map card
map_card <- function(map, height = "400px") {
  tags$div(
    class = "col-12", # Full width on all screen sizes
    tags$div(
      class = "card p-0", # Remove padding
      style = sprintf("height: %s; overflow: hidden;", height),
      tags$div(
        class = "card-body p-0", # Remove padding
        style = "height: 100%;", # Ensure full height
        map
      )
    )
  )
}
