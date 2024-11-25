icon_home <- function(size = 24) {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    class = "icon icon-tabler icon-tabler-home",
    width = size,
    height = size,
    viewbox = "0 0 24 24",
    `stroke-width` = "1.5",
    stroke = "currentColor",
    fill = "none",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    tags$path(
      stroke = "none",
      d = "M0 0h24v24H0z",
      fill = "none"
    ),
    tags$polyline(points = "5 12 3 12 12 3 21 12 19 12"),
    tags$path(d = "M5 12v7a2 2 0 0 0 2 2h10a2 2 0 0 0 2 -2v-7"),
    tags$path(d = "M9 21v-6a2 2 0 0 1 2 -2h2a2 2 0 0 1 2 2v6")
  )
}

icon_scale <- function(size = 24, class = "") {
  class <- paste("icon icon-tabler icon-tabler-scale", class)
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    class = class,
    width = "24",
    height = "24",
    viewbox = "0 0 24 24",
    `stroke-width` = "2",
    stroke = "currentColor",
    fill = "none",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    tags$path(
      stroke = "none",
      d = "M0 0h24v24H0z",
      fill = "none"
    ),
    tags$line(
      x1 = "7",
      y1 = "20",
      x2 = "17",
      y2 = "20"
    ),
    tags$path(d = "M6 6l6 -1l6 1"),
    tags$line(
      x1 = "12",
      y1 = "3",
      x2 = "12",
      y2 = "20"
    ),
    tags$path(d = "M9 12l-3 -6l-3 6a3 3 0 0 0 6 0"),
    tags$path(d = "M21 12l-3 -6l-3 6a3 3 0 0 0 6 0")
  )
}

icon_currency_dollar <- function(size = 24) {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    class = "icon icon-tabler icon-tabler-currency-dollar",
    width = size,
    height = size,
    viewbox = "0 0 24 24",
    `stroke-width` = "1.5",
    stroke = "currentColor",
    fill = "none",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    tags$path(
      stroke = "none",
      d = "M0 0h24v24H0z",
      fill = "none"
    ),
    tags$path(d = "M16.7 8a3 3 0 0 0 -2.7 -2h-4a3 3 0 0 0 0 6h4a3 3 0 0 1 0 6h-4a3 3 0 0 1 -2.7 -2"),
    tags$path(d = "M12 3v3m0 12v3")
  )
}

icon_validation <- function(size = 24) {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    class = "icon icon-tabler icon-tabler-checklist",
    width = size,
    height = size,
    viewbox = "0 0 24 24",
    `stroke-width` = "1.5",
    stroke = "currentColor",
    fill = "none",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    tags$path(
      stroke = "none",
      d = "M0 0h24v24H0z",
      fill = "none"
    ),
    tags$path(d = "M9.615 20h-2.615a2 2 0 0 1 -2 -2v-12a2 2 0 0 1 2 -2h8a2 2 0 0 1 2 2v8"),
    tags$path(d = "M14 19l2 2l4 -4"),
    tags$path(d = "M9 8h4"),
    tags$path(d = "M9 12h2")
  )
}


icon_gear <- function(size = 24, class = "") {
  class <- paste("icon icon-tabler icon-tabler-bed", class)
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    class = class,
    width = size,
    height = size,
    viewbox = "0 0 24 24",
    `stroke-width` = "1.5",
    stroke = "currentColor",
    fill = "none",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    tags$path(
      stroke = "none",
      d = "M0 0h24v24H0z",
      fill = "none"
    ),
    tags$path(d = "M10.325 4.317c.426 -1.756 2.924 -1.756 3.35 0a1.724 1.724 0 0 0 2.573 1.066c1.543 -.94 3.31 .826 2.37 2.37a1.724 1.724 0 0 0 1.065 2.572c1.756 .426 1.756 2.924 0 3.35a1.724 1.724 0 0 0 -1.066 2.573c.94 1.543 -.826 3.31 -2.37 2.37a1.724 1.724 0 0 0 -2.572 1.065c-.426 1.756 -2.924 1.756 -3.35 0a1.724 1.724 0 0 0 -2.573 -1.066c-1.543 .94 -3.31 -.826 -2.37 -2.37a1.724 1.724 0 0 0 -1.065 -2.572c-1.756 -.426 -1.756 -2.924 0 -3.35a1.724 1.724 0 0 0 1.066 -2.573c-.94 -1.543 .826 -3.31 2.37 -2.37c1 .608 2.296 .07 2.572 -1.065z"),
    tags$circle(
      cx = "12",
      cy = "12",
      r = "3"
    )
  )
}


#' Small info icon component
#' @noRd
icon_info <- function(size = 24) {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    class = "icon",
    width = size,
    height = size,
    viewBox = "0 0 24 24",
    `stroke-width` = "2",
    stroke = "currentColor",
    fill = "none",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    tags$path(stroke = "none", d = "M0 0h24v24H0z", fill = "none"),
    tags$circle(cx = "12", cy = "12", r = "9"),
    tags$line(x1 = "12", y1 = "8", x2 = "12.01", y2 = "8"),
    tags$polyline(points = "11 12 12 12 12 16 13 16")
  )
}


# Icon helper functions
icon_filter <- function(size = 24) {
  tags$svg(
    xmlns = "http://www.w3.org/2000/svg",
    class = "icon",
    width = size,
    height = size,
    viewBox = "0 0 24 24",
    `stroke-width` = "2",
    stroke = "currentColor",
    fill = "none",
    `stroke-linecap` = "round",
    `stroke-linejoin` = "round",
    tags$path(
      stroke = "none",
      d = "M0 0h24v24H0z",
      fill = "none"
    ),
    tags$path(d = "M14 6a2 2 0 1 0 -4 0a2 2 0 0 0 4 0"),
    tags$path(d = "M4 6h4"),
    tags$path(d = "M16 6h4"),
    tags$path(d = "M8 12h8"),
    tags$path(d = "M18 12h2"),
    tags$path(d = "M4 12h2"),
    tags$path(d = "M14 18a2 2 0 1 0 -4 0a2 2 0 0 0 4 0"),
    tags$path(d = "M4 18h4"),
    tags$path(d = "M16 18h4")
  )
}
