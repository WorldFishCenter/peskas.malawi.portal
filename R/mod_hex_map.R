#' Hex Map UI Module
#' @noRd
mod_hex_map_ui <- function(id) {
  ns <- NS(id)

  hex_colors <- get_hex_colors()
  color_styles <- vapply(seq_along(hex_colors), function(i) {
    sprintf(
      "flex: 1; background-color: rgb(%s);",
      paste(hex_colors[[i]], collapse = ",")
    )
  }, character(1))

  tags$div(
    class = "position-relative",
    style = "height: 500px;", # Fixed height container
    map_card(
      hexagon_map(
        data = peskas.malawi.portal::map_data,
        tracks_data = peskas.malawi.portal::tracks_df,
        color_by = "catch_kg",
        color_from = "#edf8b1",
        color_to = "#41b6c4",
        path_opacity = 0.6,
        radius = 600
      ),
      height = "100%" # Fill container
    ),
    map_info_panel(ns, color_styles)
  )
} #' Hex Map Server Module
#' @noRd
mod_hex_map_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Pre-process and cache paths data - done once at startup
    paths_data <- local({
      paths_df <- peskas.malawi.portal::tracks_df$paths
      value_range <- range(paths_df[["catch_kg"]], na.rm = TRUE)
      normalized_values <- (paths_df[["catch_kg"]] - value_range[1]) /
        (value_range[2] - value_range[1])

      col_from <- grDevices::col2rgb("#edf8b1")
      col_to <- grDevices::col2rgb("#41b6c4")

      paths_df$color <- lapply(normalized_values, function(x) {
        rgb_color <- round(col_from * (1 - x) + col_to * x)
        as.vector(rgb_color)
      })
      paths_df
    })

    # Cache hex colors and properties
    hex_colors <- get_hex_colors()
    hex_properties <- get_hex_props()

    # Debounced radius input to prevent too frequent updates
    radius_rv <- reactive(input$radius) %>%
      debounce(100)

    # Efficient proxy updates
    observeEvent(radius_rv(),
      {
        req(radius_rv())

        deckgl::deckgl_proxy(session$ns("map")) %>%
          deckgl::add_hexagon_layer(
            data = peskas.malawi.portal::map_data,
            getPosition = ~ c(lon, lat),
            radius = radius_rv(),
            colorRange = hex_colors,
            elevationRange = c(0, 3000),
            elevationScale = 80,
            extruded = TRUE,
            coverage = 0.8,
            pickable = TRUE,
            properties = hex_properties,
            tooltip = htmlwidgets::JS("window.hexagonTooltip"),
            colorAggregation = "SUM",
            upperPercentile = 95
          ) %>%
          deckgl::add_path_layer(
            data = paths_data,
            pickable = TRUE,
            getPath = ~path,
            getColor = ~ unlist(color),
            widthScale = 2,
            widthMinPixels = 1,
            widthMaxPixels = 2,
            opacity = 0.6,
            pickingRadius = 50,
            parameters = list(
              lineWidthUnits = "pixels",
              lineWidthScale = 1
            ),
            highlightColor = c(255, 255, 0, 200),
            autoHighlight = TRUE,
            billboard = TRUE,
            capRounded = TRUE,
            jointRounded = TRUE,
            tooltip = htmlwidgets::JS("window.pathTooltip")
          ) %>%
          deckgl::update_deckgl()
      },
      ignoreInit = TRUE
    )
  })
}

#' Create a hexagon heatmap using deckgl
#'
#' @description
#' Generates an interactive hexagon heatmap visualization.
#'
#' @param data A dataframe containing the map data
#' @param radius Numeric. Radius of hexagons in meters
#' @param elevation_scale Numeric. Elevation scale for 3D effect
#' @param pitch Numeric. Map pitch in degrees
#' @param zoom Numeric. Initial zoom level
#' @param color_by Character. Column name to use for coloring
#' @param color_from Character. Starting color in hex format
#' @param color_to Character. Ending color in hex format
#' @param default_color Character. Default color in hex format
#' @param path_opacity Numeric. Opacity of paths
#' @param path_width Numeric. Width of paths
#' @param tracks_data Optional dataframe containing track data
#' @return A deckgl object
#' @importFrom deckgl deckgl add_hexagon_layer add_basemap use_carto_style
#' @export
hexagon_map <- memoise::memoise(function(data = NULL,
                                         tracks_data = peskas.malawi.portal::tracks_df,
                                         radius = NULL,
                                         elevation_scale = 80,
                                         pitch = 45,
                                         zoom = 8.5,
                                         color_by = NULL,
                                         color_from = "#edf8b1",
                                         color_to = "#41b6c4",
                                         default_color = "#41b6c4",
                                         path_opacity = 0.5,
                                         path_width = 0.75) {
  # Get JS functions
  js_funcs <- get_js_functions()

  # Center coordinates
  center_lon <- 34.62791
  center_lat <- -13.7130

  # Process paths data
  paths_data <- if (!is.null(color_by)) {
    paths_df <- tracks_data$paths
    if (is.numeric(paths_df[[color_by]])) {
      value_range <- range(paths_df[[color_by]], na.rm = TRUE)
      normalized_values <- (paths_df[[color_by]] - value_range[1]) /
        (value_range[2] - value_range[1])
      col_from <- grDevices::col2rgb(color_from)
      col_to <- grDevices::col2rgb(color_to)
      paths_df$color <- lapply(normalized_values, function(x) {
        rgb_color <- round(col_from * (1 - x) + col_to * x)
        as.vector(rgb_color)
      })
    } else {
      paths_df$color <- list(as.vector(grDevices::col2rgb(default_color)))
    }
    paths_df
  } else {
    paths_df <- tracks_data$paths
    paths_df$color <- list(as.vector(grDevices::col2rgb(default_color)))
    paths_df
  }

  # Create the map
  map <- deckgl::deckgl(
    longitude = center_lon,
    latitude = center_lat,
    zoom = zoom,
    pitch = pitch,
    width = "100%",
    height = "100%",
    style = list(position = "absolute", top = 0, left = 0, right = 0, bottom = 0)
  ) %>%
    deckgl::add_basemap(
      # style = "mapbox://styles/langbart/cli8oua4m002a01pg17wt6vqa",
      # token = get_pars()$mapbox_token
    ) %>%
    # Add hexagon layer
    deckgl::add_hexagon_layer(
      data = data,
      getPosition = ~ c(lon, lat), # Changed from c(center_lon, center_lat) to c(lon, lat)
      colorRange = get_hex_colors(),
      elevationRange = c(0, 3000),
      elevationScale = elevation_scale,
      extruded = TRUE,
      radius = radius,
      coverage = 0.8,
      pickable = TRUE,
      properties = get_hex_props(),
      tooltip = htmlwidgets::JS("window.hexagonTooltip"),
      colorAggregation = "SUM",
      upperPercentile = 95
    ) %>%
    # Add path layer
    deckgl::add_path_layer(
      data = paths_data,
      pickable = TRUE,
      getPath = ~path,
      getColor = ~ unlist(color),
      widthScale = 2,
      widthMinPixels = 1,
      widthMaxPixels = 2,
      opacity = path_opacity,
      pickingRadius = 50,
      parameters = list(
        lineWidthUnits = "pixels",
        lineWidthScale = 1
      ),
      highlightColor = c(255, 255, 0, 200),
      autoHighlight = TRUE,
      billboard = TRUE,
      capRounded = TRUE,
      jointRounded = TRUE,
      tooltip = htmlwidgets::JS("window.pathTooltip")
    )

  htmltools::attachDependencies(map, add_js_dependencies())
})

#' Helper functions - cached at package level for reuse
#' @noRd
get_hex_colors <- memoise::memoise(function() {
  list(
    c(1, 152, 189),
    c(73, 227, 206),
    c(216, 254, 181),
    c(254, 237, 177),
    c(254, 173, 84),
    c(209, 55, 78)
  )
})

#' @noRd
get_hex_props <- memoise::memoise(function() {
  list(
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
})


#' Create info panel for hex map
#' @param ns Namespace function
#' @param color_styles Pre-calculated color styles
#' @noRd
map_info_panel <- function(ns, color_styles) {
  tags$div(
    class = "card shadow-sm",
    style = "position: absolute; top: 1rem; right: 1rem; width: 300px; background: rgba(255,255,255,0.75); z-index: 1000; border: none; border-radius: 0.5rem; max-height: calc(100% - 2rem); overflow-y: auto; backdrop-filter: blur(2px);", # Changed alpha to 0.85 and added blur
    tags$div(
      class = "card-body p-3",
      # Title
      tags$h3(
        class = "card-title mb-2",
        style = "font-size: 1.2rem; font-weight: 600;",
        "Fishing Activity",
        tags$span(
          class = "ms-1",
          style = "cursor: help;",
          `data-bs-toggle` = "tooltip",
          title = "This map shows fishing vessel activity and catch distribution across Malawi waters",
          icon_info()
        )
      ),

      # Description
      tags$p(
        class = "text-muted mb-3",
        style = "font-size: 0.85rem;",
        "Visualizing fishing vessel density and catch amounts in Malawi waters."
      ),

      # Activity Density
      tags$h4(
        class = "mb-2",
        style = "font-size: 0.9rem; font-weight: 600; color: #666;",
        "Fishing Activity Density"
      ),
      tags$div(
        class = "mb-1",
        style = "display: flex; height: 20px;",
        lapply(seq_along(color_styles), function(i) {
          tags$div(style = color_styles[i])
        })
      ),
      tags$div(
        class = "d-flex justify-content-between text-muted mb-3",
        style = "font-size: 0.8rem;",
        tags$span("Low Activity"),
        tags$span("High Activity")
      ),

      # Tracks
      tags$h4(
        class = "mb-2",
        style = "font-size: 0.9rem; font-weight: 600; color: #666;",
        "Fishing Tracks",
        tags$span(
          class = "ms-1",
          style = "cursor: help;",
          `data-bs-toggle` = "tooltip",
          title = "Lines represent individual fishing vessel trips. Color intensity indicates catch amount.",
          icon_info()
        )
      ),
      tags$div(
        style = "display: flex; height: 20px;",
        tags$div(style = sprintf("flex: 1; background: linear-gradient(to right, %s, %s);", "#edf8b1", "#41b6c4"))
      ),
      tags$div(
        class = "d-flex justify-content-between text-muted mb-3",
        style = "font-size: 0.8rem;",
        tags$span("Low Catch"),
        tags$span("High Catch")
      ),

      # Controls
      tags$h4(
        class = "mb-2",
        style = "font-size: 0.9rem; font-weight: 600; color: #666;",
        "Visualization Controls"
      ),
      tags$div(
        class = "d-flex align-items-center justify-content-between mb-1",
        tags$div(
          style = "font-size: 0.8rem; color: #666;",
          tags$span("Hex Radius (meters)"),
        )
      ),
      sliderInput(
        inputId = ns("radius"),
        label = NULL,
        min = 200,
        max = 1200,
        value = 600,
        step = 100,
        ticks = FALSE
      ),

      # Hint
      tags$p(
        class = "text-muted mb-0 mt-2",
        style = "font-size: 0.8rem; border-top: 1px solid #eee; padding-top: 0.5rem;",
        "Hover over hexagons and tracks for detailed information"
      )
    )
  )
}
