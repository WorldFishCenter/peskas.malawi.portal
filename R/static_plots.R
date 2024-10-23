#' Create a homecard time series plot
#'
#' @param data A dataframe containing the time series data
#' @param x_col The name of the column to be used for the x-axis (default: "date_month")
#' @param y_col The name of the column to be used for the y-axis
#' @param type The type of plot (default: "column")
#'
#' @return An apexchart object
#'
#' @importFrom apexcharter apex ax_chart ax_xaxis ax_yaxis ax_grid ax_colors ax_stroke
#' @importFrom rlang sym
homecard_ts_plot <- function(data, x_col = "date_month", y_col, type = "area") {
  data %>%
    apexcharter::apex(
      apexcharter::aes(x = !!rlang::sym(x_col), y = !!rlang::sym(y_col)),
      type = type
    ) %>%
    apexcharter::ax_chart(
      toolbar = list(show = FALSE),
      sparkline = list(enabled = TRUE)
    ) %>%
    apexcharter::ax_xaxis(
      labels = list(show = FALSE),
      axisTicks = list(show = FALSE),
      axisBorder = list(show = FALSE)
    ) %>%
    apexcharter::ax_yaxis(
      labels = list(show = FALSE),
      axisTicks = list(show = FALSE),
      axisBorder = list(show = FALSE)
    ) %>%
    apexcharter::ax_grid(show = FALSE) %>%
    apexcharter::ax_colors("#41b6c4") %>% # You can change this color as needed
    apexcharter::ax_stroke(width = 1) %>% # Adjust the width of the columns
    apexcharter::ax_chart(
      animations = list(
        enabled = FALSE # Disable animations for better performance in small cards
      )
    )
}



#' Generate a hexagon heatmap using mapdeck
#'
#' @param data A dataframe containing the map data with 'lat' and 'lon' columns
#' @param style Mapdeck style (default: "dark")
#' @param radius Radius of hexagons in meters (default: 2000)
#' @param elevation_scale Elevation scale for 3D effect (default: 250)
#' @param pitch Map pitch in degrees (default: 45)
#' @param zoom Initial zoom level (default: 7)
#'
#' @return A mapdeck object
#' @import mapdeck
#' @import viridisLite
hexagon_map <- function(data,
                        style = "dark",
                        radius = 2000,
                        elevation_scale = 250,
                        pitch = 45,
                        zoom = 7) {
  # Calculate center coordinates
  center_lon <- stats::median(data$lon, na.rm = TRUE)
  center_lat <- stats::median(data$lat, na.rm = TRUE)

  # Generate the map
  mapdeck::mapdeck(
    style = mapdeck::mapdeck_style(style)
  ) %>%
    mapdeck::add_hexagon(
      data = data,
      lat = "lat",
      lon = "lon",
      layer_id = "heatmap_layer",
      radius = radius,
      colour_range = RColorBrewer::brewer.pal(6, "YlGnBu"),
      elevation_scale = elevation_scale,
      auto_highlight = TRUE
    ) %>%
    mapdeck::mapdeck_view(
      pitch = pitch,
      location = c(center_lon, center_lat),
      zoom = zoom
    )
}

#' Create a styled district summary table
#'
#' @param data A dataframe containing the summary data
#' @param color_pal Color palette for the background gradient (default: c("#f7fbff", "#08306b"))
#'
#' @return A reactable object
#' @import reactable
#' @import dplyr
#' @import scales
district_summary_table <- function(data, color_pal = c("#ffffd9", "#c7e9b4", "#41b6c4")) {
  make_color_pal <- function(colors, bias = 1) {
    get_color <- grDevices::colorRamp(colors, bias = bias)
    function(x) grDevices::rgb(get_color(x), maxColorValue = 255)
  }

  good_color <- make_color_pal(color_pal, bias = 2)

  reactable::reactable(
    data,
    theme = reactablefmtr::fivethirtyeight(centered = TRUE),
    pagination = FALSE,
    compact = TRUE,
    borderless = FALSE,
    striped = FALSE,
    fullWidth = TRUE,
    sortable = TRUE,
    defaultSorted = "sample_district",
    defaultColDef = reactable::colDef(
      align = "center",
      minWidth = 100
    ),
    columns = list(
      sample_district = reactable::colDef(
        name = "District",
        minWidth = 140,
        align = "left"
      ),
      catch_kg = reactable::colDef(
        name = "Catch (kg)",
        format = reactable::colFormat(digits = 1),
        style = function(value) {
          normalized <- (value - min(data$catch_kg)) / (max(data$catch_kg) - min(data$catch_kg))
          color <- good_color(normalized)
          list(background = color)
        }
      ),
      catch_price = reactable::colDef(
        name = "Catch Value (MWK)",
        format = reactable::colFormat(digits = 0, separators = TRUE),
        cell = function(value) {
          scales::dollar(value, prefix = "MWK ", largest_with_cents = 1)
        },
        style = function(value) {
          normalized <- (value - min(data$catch_price)) / (max(data$catch_price) - min(data$catch_price))
          color <- good_color(normalized)
          list(background = color)
        }
      ),
      price_kg_USD = reactable::colDef(
        name = "Price per kg (USD)",
        format = reactable::colFormat(digits = 2),
        cell = function(value) {
          scales::dollar(value)
        },
        style = function(value) {
          normalized <- (value - min(data$price_kg_USD)) / (max(data$price_kg_USD) - min(data$price_kg_USD))
          color <- good_color(normalized)
          list(background = color)
        }
      )
    )
  )
}
