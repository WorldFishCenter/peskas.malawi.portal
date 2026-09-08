footer_panel <- function(right_side_elements = tagList(), left_side_elements = tagList(), logos = NULL, bottom = "") {
  tags$footer(
    class = "footer footer-transparent d-print-none",
    tags$div(
      class = "container",
      tags$div(
        class = "row text-center align-items-center flex-row-reverse",
        tags$div(
          class = "col-lg-auto ms-lg-auto",
          tags$ul(
            class = "list-inline list-inline-dots mb-0",
            right_side_elements
          )
        ),
        tags$div(
          class = "col-12 col-lg-auto mt-3 mt-lg-0",
          tags$ul(
            class = "list-inline list-inline-dots mb-0",
            left_side_elements
          )
        )
      ),
      logos,
      tags$div(
        class = "row text-center align-items-center flex-row-reverse mt-3",
        tags$div(
          class = "col-lg",
          tags$p(
            bottom
          )
        )
      ),
    )
  )
}


#' Creates a single partner or funder logo for the footer
#'
#' @param src Image path, relative to the "www" resource prefix registered by
#'   `tabler_page()` (i.e. files living in `inst/app/www`).
#' @param alt Organisation name, used as the alternative text and tooltip.
#' @param class Optional extra CSS class. Use "footer-logo-wide" for stacked or
#'   horizontal lockups that carry a caption, so the wordmark stays legible.
#'
#' @return a shiny tag "img" element
#' @noRd
footer_logo <- function(src, alt, class = NULL) {
  tags$img(
    src = src,
    alt = alt,
    title = alt,
    class = paste(c("footer-logo", class), collapse = " "),
    loading = "lazy"
  )
}

#' Creates a centred row of partner or funder logos for the footer
#'
#' @param ... Logos created with `footer_logo()`
#'
#' @return a shiny tag "div" element, or NULL when no logo is given
#' @noRd
footer_logos <- function(...) {
  logos <- list(...)
  if (length(logos) == 0) {
    return(NULL)
  }

  tags$div(
    class = "row mt-3",
    tags$div(
      class = "col-12 footer-logos",
      logos
    )
  )
}


#' Creates a inline list item with a link
#'
#' Te be used in elements like the footer
#'
#'
#' @param content Link
#' @param href Prefix to reference
#' @param target Target reference
#'
#' @return a shiny tag "li" element
#' @export
#'
inline_li_link <- function(content = "Link text", href = "#", target = "_blank") {
  tags$li(
    class = "list-inline-item",
    tags$a(
      href = href,
      target = target,
      class = "link-secondary",
      content
    )
  )
}
