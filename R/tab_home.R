#' Generate content for the home tab of the Malawi small scale fisheries dashboard
#'
#' @description
#' This function creates the layout and content for the home tab of the dashboard.
#' It includes a page heading, three small cards with time series plots, a map card,
#' and a district summary table.
#'
#' @return A tagList containing the structured content for the home tab
#'
#' @details
#' The function uses the following components:
#' - page_heading: Sets the title and subtitle for the page
#' - small_card: Creates cards for number of submissions, vessels surveyed, and catches recorded
#' - homecard_ts_plot: Generates time series plots for each small card
#' - map_card: Displays a hexagon map of the data
#' - hexagon_map: Creates the hexagon map using Mapbox
#' - district_summary_table: Generates a summary table of district data
#'
#' @note
#' Requires access to peskas.malawi.portal data and a valid Mapbox token
#'
#' export
tab_home_content <- function() {
  tagList(
    page_heading(
      pretitle = "small scale fisheries",
      title = "Home"
    ),
    alert(
      title = "Beta Version",
      message = "This application is currently in beta testing phase."
    ),
    page_cards(
      # First row - Small cards
      tags$div(
        class = "row g-2",
        tags$div(
          class = "col-12",
          tags$div(
            class = "row g-3", # Nested row for small cards
            small_card(
              title = "Number of submissions",
              plot = homecard_ts_plot_memo(peskas.malawi.portal::homecards_plots$submissions,
                y_col = "N. submissions"
              )
            ),
            small_card(
              title = "Vessels surveyed",
              plot = homecard_ts_plot_memo(peskas.malawi.portal::homecards_plots$vessels,
                y_col = "Vessels surveyed"
              )
            ),
            small_card(
              title = "Catches recorded",
              plot = homecard_ts_plot_memo(peskas.malawi.portal::homecards_plots$catches,
                y_col = "Catches recorded"
              )
            )
          )
        )
      ),
      # Second row - Map
      tags$div(
        class = "row g-2",
        tags$div(
          class = "col-12",
          map_card(
            hexagon_map(
              peskas.malawi.portal::map_data
            ),
            height = "500px"
          )
        )
      ),
      # Third row - District summary table
      tags$div(
        class = "row g-2",
        tags$div(
          class = "col-12",
          card(
            title = "Districts summary",
            tooltip = "
              <p>This table shows the average values per fishing trip for each district:</p>
              <ul class='mb-0'>
                <li>Catch: Average catch per trip (kg)</li>
                <li>CPUE: Average catch per fisher per hour (kg/fisher/hr)</li>
                <li>Catch Value: Average catch value per trip</li>
                <li>Price per kg: Average price per kilogram</li>
                <li>N. fishers: Average number of fishers per trip</li>
                <li>Trip length: Average trip duration (hrs)</li>
              </ul>",
            mod_district_summary_table_ui("district_table")
          )
        )
      )
    )
  )
}
