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


#' Generate a hexagon heatmap using deckgl
#'
#' @param data A dataframe containing the map data with 'lat' and 'lon' columns
#' @param radius Radius of hexagons in meters (default: 3000)
#' @param elevation_scale Elevation scale for 3D effect (default: 80)
#' @param pitch Map pitch in degrees (default: 45)
#' @param zoom Initial zoom level (default: 7)
#'
#' @return A deckgl object
#' @import deckgl
hexagon_map <- function(data,
                        radius = 3000,
                        elevation_scale = 80,
                        pitch = 45,
                        zoom = 6.5) {
  # Calculate center coordinates
  center_lon <- stats::median(data$lon, na.rm = TRUE)
  center_lat <- stats::median(data$lat, na.rm = TRUE)

  # Define color range
  color_range <- list(
    c(1, 152, 189), # Light blue
    c(73, 227, 206), # Turquoise
    c(216, 254, 181), # Light green
    c(254, 237, 177), # Light yellow
    c(254, 173, 84), # Orange
    c(209, 55, 78) # Red
  )

  # Define material properties
  props <- list(
    autoHighlight = TRUE,
    material = list(
      ambient = 0.65,
      diffuse = 0.35,
      specularColor = c(51, 51, 51)
    ),
    transitions = list(
      elevationScale = 3000
    )
  )

  # Simple tooltip
  tooltip_js <- htmlwidgets::JS("object => {
      const totalActivities = object.points.length;
      return `
        <div padding: 10px; border-radius: 5px;'>
          <strong>Location:</strong> ${object.position[1].toFixed(3)}, ${object.position[0].toFixed(3)}<br>
          <strong>Total Activities:</strong> ${totalActivities}
        </div>
      `;
    }")

  # Generate the map with correct sizing for your card system
  deckgl::deckgl(
    longitude = center_lon,
    latitude = center_lat,
    zoom = zoom,
    pitch = pitch,
    width = "100%",
    height = "100%",
    style = list(position = "absolute", top = 0, left = 0, right = 0, bottom = 0)
  ) %>%
    deckgl::add_basemap(
      style = deckgl::use_carto_style(theme = "dark-matter")
    ) %>%
    deckgl::add_hexagon_layer(
      data = data,
      getPosition = ~ c(lon, lat),
      colorRange = color_range,
      elevationRange = c(0, 3000),
      elevationScale = elevation_scale,
      extruded = TRUE,
      radius = radius,
      coverage = 0.8,
      pickable = TRUE,
      properties = props,
      tooltip = tooltip_js,
      colorAggregation = "SUM",
      upperPercentile = 95
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
      `Catch (kg)` = reactable::colDef(
        name = "Catch (kg)",
        format = reactable::colFormat(digits = 1),
        style = function(value) {
          normalized <- (value - min(data$`Catch (kg)`)) / (max(data$`Catch (kg)`) - min(data$`Catch (kg)`))
          color <- good_color(normalized)
          list(background = color)
        }
      ),
      `Catch Value (MWK)` = reactable::colDef(
        name = "Catch Value (MWK)",
        format = reactable::colFormat(digits = 0, separators = TRUE),
        cell = function(value) {
          scales::dollar(value, prefix = "MWK ", largest_with_cents = 1)
        },
        style = function(value) {
          normalized <- (value - min(data$`Catch Value (MWK)`)) / (max(data$`Catch Value (MWK)`) - min(data$`Catch Value (MWK)`))
          color <- good_color(normalized)
          list(background = color)
        }
      ),
      `Price per kg (USD)` = reactable::colDef(
        name = "Price per kg (USD)",
        format = reactable::colFormat(digits = 2),
        cell = function(value) {
          scales::dollar(value)
        },
        style = function(value) {
          normalized <- (value - min(data$`Price per kg (USD)`)) / (max(data$`Price per kg (USD)`) - min(data$`Price per kg (USD)`))
          color <- good_color(normalized)
          list(background = color)
        }
      )
    )
  )
}
