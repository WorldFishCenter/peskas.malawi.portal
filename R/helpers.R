#' Create a Tabler-styled dismissible alert
#'
#' @param title Alert title
#' @param message Alert message
#' @param type Alert type ("warning", "info", "success", "danger")
#'
#' @return A shiny tag list
alert <- function(title, message, type = "warning") {
  tags$div(
    class = "container-xl", # Match page container
    tags$div(
      class = "alert alert-warning alert-dismissible", # Added alert-dismissible
      role = "alert",
      tags$div(
        class = "d-flex",
        tags$div(
          tags$svg(
            xmlns = "http://www.w3.org/2000/svg",
            width = "24",
            height = "24",
            viewbox = "0 0 24 24",
            fill = "none",
            stroke = "currentColor",
            `stroke-width` = "2",
            `stroke-linecap` = "round",
            `stroke-linejoin` = "round",
            class = "icon alert-icon",
            tags$path(
              stroke = "none",
              d = "M0 0h24v24H0z",
              fill = "none"
            ),
            tags$path(d = "M12 9v4"),
            tags$path(d = "M10.363 3.591l-8.106 13.534a1.914 1.914 0 0 0 1.636 2.871h16.214a1.914 1.914 0 0 0 1.636 -2.87l-8.106 -13.536a1.914 1.914 0 0 0 -3.274 0z"),
            tags$path(d = "M12 16h.01")
          )
        ),
        tags$div(
          tags$h4(
            class = "alert-title",
            title
          ),
          tags$div(
            class = "text-secondary",
            message
          )
        )
      ),
      tags$a(
        class = "btn-close",
        `data-bs-dismiss` = "alert",
        `aria-label` = "close"
      )
    )
  )
}
