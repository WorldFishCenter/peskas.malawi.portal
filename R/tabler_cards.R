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

card <- function(title = "Card title", ..., tooltip = NULL) {
  # Add custom CSS for transparent tooltip
  tooltip_css <- tags$style("
    .popover {
      background-color: rgba(255, 255, 255, 0.6) !important;
      backdrop-filter: blur(3px);
    }
    .popover .arrow::after {
      border-right-color: rgba(255, 255, 255, 0.9) !important;
    }
  ")

  tooltip_element <- if (!is.null(tooltip)) {
    tagList(
      tooltip_css,
      tags$span(
        class = "form-help ms-2",
        `data-bs-toggle` = "popover",
        `data-bs-trigger` = "hover focus",
        `data-bs-placement` = "right",
        `data-bs-html` = "true",
        `data-bs-content` = tooltip,
        "?"
      )
    )
  } else {
    NULL
  }

  tags$div(
    class = "col-12",
    tags$div(
      class = "card",
      tags$div(
        class = "card-body",
        tags$h3(
          class = "card-title",
          title,
          tooltip_element
        ),
        ...
      )
    )
  )
}

#' Create a small card with an embedded plot that fills the entire card and ensures visible tooltip
#'
#' @param title The title of the card
#' @param plot The apexchart object to embed
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
    class = "card p-0",
    style = sprintf("height: %s; position: relative;", height), # Add relative positioning here
    tags$div(
      class = "card-body p-0",
      style = "height: 100%;",
      map
    )
  )
}


#' Create district selector
#'
#' @param id ID for the selector (e.g., "catch-district" or "revenue-district")
#' @param data Dataset containing the sample_district column
#' @param width Optional width for the selector container (default: "col-md-4 col-lg-3")
#'
#' @return A div containing the styled district selector
#' @noRd
district_selector <- function(id, data = peskas.malawi.portal::timeseries_month, width = "col-md-4 col-lg-3") {
  tags$div(
    class = "container-xl mb-3",
    tags$div(
      class = "row",
      tags$div(
        class = width,
        selectInput(
          inputId = id,
          label = "Select District",
          choices = c(
            "All districts",
            setdiff(
              unique(data$sample_district),
              "All districts"
            )
          ),
          selected = "All districts"
        )
      )
    )
  )
}
