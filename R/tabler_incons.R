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
