#' Create a homecard time series plot
#'
#' @param data A dataframe containing the time series data
#' @param x_col The name of the column to be used for the x-axis (default: "date_month")
#' @param y_col The name of the column to be used for the y-axis
#' @param type The type of plot (default: "area")
#'
#' @return An apexchart object
#'
homecard_ts_plot <- function(data, x_col = "date_month", y_col, type = "area") {
  # Pre-compute the symbols
  y_sym <- rlang::sym(y_col)
  x_sym <- rlang::sym(x_col)

  data %>%
    apexcharter::apex(
      apexcharter::aes(x = !!x_sym, y = !!y_sym),
      type = type
    ) %>%
    apexcharter::ax_chart(
      toolbar = list(show = FALSE),
      sparkline = list(enabled = TRUE),
      animations = list(enabled = FALSE)
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
    apexcharter::ax_colors("#41b6c4") %>%
    apexcharter::ax_stroke(width = 1)
}

#' Memoised version of homecard_ts_plot
#'
#' @inheritParams homecard_ts_plot
#' @export
homecard_ts_plot_memo <- memoise::memoise(homecard_ts_plot)


#' Generate a hexagon heatmap using deckgl
#'
#' @param data A dataframe containing the map data with 'lat' and 'lon' columns
#' @param radius Radius of hexagons in meters (default: 3000)
#' @param elevation_scale Elevation scale for 3D effect (default: 80)
#' @param pitch Map pitch in degrees (default: 45)
#' @param zoom Initial zoom level (default: 7)
#'
hexagon_map <- memoise::memoise(function(data,
                                         radius = 3000,
                                         elevation_scale = 80,
                                         pitch = 45,
                                         zoom = 6.5) {
  # Cache center calculations
  center_lon <- stats::median(data$lon, na.rm = TRUE)
  center_lat <- stats::median(data$lat, na.rm = TRUE)

  # Static configurations
  color_range <- list(
    c(1, 152, 189),
    c(73, 227, 206),
    c(216, 254, 181),
    c(254, 237, 177),
    c(254, 173, 84),
    c(209, 55, 78)
  )

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

  # Cache the tooltip function
  tooltip_js <- memoise::memoise(function() {
    htmlwidgets::JS("object => {
      const totalActivities = object.points.length;
      return `
        <div padding: 10px; border-radius: 5px;'>
          <strong>Location:</strong> ${object.position[1].toFixed(3)}, ${object.position[0].toFixed(3)}<br>
          <strong>Total Activities:</strong> ${totalActivities}
        </div>
      `;
    }")
  })

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
      tooltip = tooltip_js(),
      colorAggregation = "SUM",
      upperPercentile = 95
    )
})

#' Create a styled district summary table
#'
#' @param data A dataframe containing the summary data
#' @param color_pal Color palette for the background gradient (default: c("#ffffd9", "#c7e9b4", "#41b6c4"))
#'
#' @return A reactable object
district_summary_table <- memoise::memoise(function(data,
                                                    color_pal = c("#ffffd9", "#c7e9b4", "#41b6c4")) {
  # Cache the color palette function
  good_color <- grDevices::colorRamp(color_pal, bias = 2)

  # Pre-calculate min/max values
  catch_range <- range(data$`Catch (kg)`)
  value_range <- range(data$`Catch Value (MWK)`)
  price_range <- range(data$`Price per kg (USD)`)

  # Create reusable normalization function
  normalize <- function(x, range) {
    (x - range[1]) / (range[2] - range[1])
  }

  # Create style function factory
  make_style_fn <- function(range) {
    function(value) {
      normalized <- normalize(value, range)
      color <- grDevices::rgb(good_color(normalized), maxColorValue = 255)
      list(background = color)
    }
  }

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
        style = make_style_fn(catch_range)
      ),
      `Catch Value (MWK)` = reactable::colDef(
        name = "Catch Value (MWK)",
        format = reactable::colFormat(digits = 0, separators = TRUE),
        cell = function(value) scales::dollar(value, prefix = "MWK ", largest_with_cents = 1),
        style = make_style_fn(value_range)
      ),
      `Price per kg (USD)` = reactable::colDef(
        name = "Price per kg (USD)",
        format = reactable::colFormat(digits = 2),
        cell = function(value) scales::dollar(value),
        style = make_style_fn(price_range)
      )
    )
  )
})
