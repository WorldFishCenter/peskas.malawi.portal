#' Create a tabset panel
#'
#' Creates a panel of tabs that is controled with `tab_menu()`
#'
#' @param ... panels created with `tab_panel()`
#' @param menu_id HTML element ID
#'
#' @return a shiny.tag object
#' @seealso tab_menu
tabset_panel <- function(..., menu_id = "") {
  panels <- list(...)
  if (length(panels) >= 1) {
    panels[[1]] <- tagAppendAttributes(panels[[1]], class = "active show")
  }

  tags$div(
    class = "tab-content page-wrapper",
    `data-tabsetid` = menu_id,
    panels
  )
}

#' Create a panel
#'
#' @param ... content of the panel
#' @param id id of the panel. Must match the one created with `tab_menu_item()`
#'
#' @return a shiny.tag object
#' @seealso tabset_panel
tab_panel <- function(..., id = "") {
  tags$div(
    class = "tab-pane fade",
    id = id,
    role = "tabpanel",
    `data-value` = id,
    `aria-labelled-by` = paste0(id, "-menu"),
    ...
  )
}



tabler_nav_dropdown <- function(..., label, icon = NULL) {
  tags$li(
    class = "nav-item dropdown",
    tags$a(
      class = "nav-link dropdown-toggle",
      `data-bs-toggle` = "dropdown",
      href = "#",
      role = "button",
      `aria-expanded` = "false",
      tags$span(
        class = "nav-link-icon d-md-none d-lg-inline-block",
        icon
      ),
      tags$span(
        class = "nav-link-title",
        label
      )
    ),
    tags$div(
      class = "dropdown-menu",
      ...
    )
  )
}


dropdown_item <- function(label, nav_target) {
  tags$a(
    class = "dropdown-item",
    `data-bs-toggle` = "tab",
    `data-bs-target` = paste0("#", nav_target),
    id = paste0(nav_target, "-tab"),
    `aria-controls` = nav_target,
    href = paste0("#", nav_target),
    label
  )
}
