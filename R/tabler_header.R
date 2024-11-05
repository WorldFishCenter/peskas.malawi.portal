#' Add a header
#'
#' @param logo html for the logo
#' @param ... html tags to be included on the right hand side of the header
#'
header <- function(..., logo = NULL) {
  tags$a(
    href = ".",
    logo
  )
}


#' Create sticky navigation structure
#' @noRd
stick_nav <- function(header_content, menu_content) {
  tags$div(
    class = "sticky-top",
    # First header with logo and right elements
    tags$header(
      class = "navbar navbar-expand-md navbar-light d-print-none",
      tags$div(
        class = "container-xl",
        tags$button(
          class = "navbar-toggler",
          type = "button",
          `data-bs-toggle` = "collapse",
          `data-bs-target` = "#navbar-menu",
          tags$span(class = "navbar-toggler-icon")
        ),
        tags$h1(
          class = "navbar-brand navbar-brand-autodark d-none-navbar-horizontal pe-0 pe-md-3",
          header_content
        )
      )
    ),
    # Second header with navigation menu
    tags$header(
      class = "navbar-expand-md",
      tags$div(
        class = "collapse navbar-collapse",
        id = "navbar-menu",
        tags$div(
          class = "navbar",
          tags$div(
            class = "container-xl",
            menu_content
          )
        )
      )
    )
  )
}

#' Create a navigation menu
#'
#' Creates a navigation menu that controls tab panels created with
#' `tabset_panel()`. The id of the menu and the panel must match.
#'
#' @param ... Navigation menu items created with `navigation_menu_item()`
#' @param id HTML element ID
#'
#' @return a shiny tag
#'
tab_menu <- function(..., id = "") {
  menu_items <- list(...)
  if (length(menu_items) >= 1) {
    menu_items[[1]]$children[[1]] <-
      tagAppendAttributes(menu_items[[1]]$children[[1]], class = "active")
  }

  tags$ul(
    class = "nav navbar-nav shiny-tab-input",
    id = id,
    `data-tabsetid` = id,
    role = "tablist",
    menu_items
  )
}


#' Create a navigation menu item
#'
#' @param label Text to display
#' @param id unique tab-name
#' @param icon_svg icon for the menu
#'
#' @seealso tab_menu
tab_menu_item <- function(label = "", id = "", icon_svg = NULL) {
  icon <- if (!is.null(icon_svg)) {
    tags$span(
      class = "nav-link-icon d-md-none d-lg-inline-block",
      icon_svg
    )
  } else {
    list()
  }

  tags$li(
    class = "nav-item",
    role = "presentation",
    tags$a(
      class = "nav-link",
      id = paste0(id, "-menu"),
      `data-bs-toggle` = "tab",
      `data-bs-target` = paste0("#", id),
      `aria-controls` = id,
      `data-value` = id,
      `data-toggle` = "tab",
      href = paste0("#", id),
      icon,
      tags$span(
        class = "nav-link-title",
        label
      )
    )
  )
}



peskas_logo <- function() {
  htmltools::tags$div(
    htmltools::tags$span(
      class = "text-blue logo",
      style = "font-weight: bolder; display: inline-block; vertical-align: middle;",
      "PESKAS\u2122"
    ),
    htmltools::tags$img(width = "30", height = "20", style = "display: inline-block; vertical-align: middle; margin-right: 5px;", src = "https://upload.wikimedia.org/wikipedia/commons/d/d1/Flag_of_Malawi.svg")
  )
}

version_flex <- function(heading = "Heading",
                         subheading = "subheading") {
  htmltools::tags$div(
    class = "nav-item d-none d-md-flex me-3",
    htmltools::tags$span(heading),
    htmltools::tags$small(
      class = "text-muted",
      subheading
    )
  )
}
