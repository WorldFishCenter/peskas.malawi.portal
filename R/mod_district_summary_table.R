#' District Summary Table UI
#' @noRd
mod_district_summary_table_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$div(
      class = "currency-selector mb-3",
      selectInput(
        ns("currency"),
        "Select Currency",
        choices = c("MWK", "USD", "EUR", "GBP"),
        selected = "MWK"
      )
    ),
    reactable::reactableOutput(ns("table"))
  )
}

#' District Summary Table Server
#' @noRd
mod_district_summary_table_server <- function(id, data, color_pal = c("#ffffd9", "#c7e9b4", "#41b6c4")) {
  moduleServer(id, function(input, output, session) {
    # Define available currencies and their conversion rates
    currencies <- list(
      MWK = list(rate = 1, symbol = "MWK", decimals = 0), # No decimals for MWK
      USD = list(rate = 0.00058, symbol = "$", decimals = 2), # 2 decimals for USD
      EUR = list(rate = 0.00053, symbol = "\u20AC", decimals = 2), # Euro symbol
      GBP = list(rate = 0.00045, symbol = "\u00A3", decimals = 2) # Pound symbol
    )

    # Convert the data reactively based on selected currency
    converted_data <- reactive({
      req(input$currency)
      curr <- input$currency
      rate <- currencies[[curr]]$rate

      data %>%
        dplyr::select(
          "sample_district",
          "Catch (kg)",
          "Catch per unit effort (kg)",
          "Catch Value (MWK)",
          "Price per kg (MWK)",
          "N. fishers",
          "Trip length (hrs)"
        ) %>%
        dplyr::transmute(
          sample_district = .data$sample_district,
          Catch = round(.data$`Catch (kg)`, 1),
          CPUE = round(.data$`Catch per unit effort (kg)`, 2),
          `Catch Value` = .data$`Catch Value (MWK)` * rate,
          `Price per kg` = .data$`Price per kg (MWK)` * rate,
          `N. fishers` = round(.data$`N. fishers`, 1),
          `Trip length` = round(.data$`Trip length (hrs)`, 1)
        )
    })

    # Color and styling functions
    good_color <- grDevices::colorRamp(color_pal, bias = 2)

    normalize <- function(x, range) {
      (x - range[1]) / (range[2] - range[1])
    }

    make_style_fn <- function(range) {
      function(value) {
        normalized <- normalize(value, range)
        color <- grDevices::rgb(good_color(normalized), maxColorValue = 255)
        list(background = color)
      }
    }

    output$table <- reactable::renderReactable({
      curr_data <- converted_data()
      curr <- input$currency
      symbol <- currencies[[curr]]$symbol
      decimals <- currencies[[curr]]$decimals

      # Pre-calculate ranges
      catch_range <- range(curr_data$Catch)
      value_range <- range(curr_data$`Catch Value`)
      price_range <- range(curr_data$`Price per kg`)
      cpue_range <- range(curr_data$CPUE)
      fishers_range <- range(curr_data$`N. fishers`)
      trip_range <- range(curr_data$`Trip length`)

      reactable::reactable(
        curr_data,
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
          Catch = reactable::colDef(
            format = reactable::colFormat(digits = 1),
            cell = function(value) paste(value, "kg"),
            style = make_style_fn(catch_range)
          ),
          CPUE = reactable::colDef(
            format = reactable::colFormat(digits = 2),
            cell = function(value) paste(value, "kg"),
            style = make_style_fn(cpue_range)
          ),
          `Catch Value` = reactable::colDef(
            format = reactable::colFormat(digits = decimals, separators = TRUE),
            cell = function(value) {
              formatted_value <- if (curr == "MWK") {
                scales::comma(round(value, 0))
              } else {
                scales::dollar(value, digits = 2, prefix = "") # Using scales::dollar for proper decimal formatting
              }
              paste(symbol, formatted_value)
            },
            style = make_style_fn(value_range)
          ),
          `Price per kg` = reactable::colDef(
            format = reactable::colFormat(digits = decimals),
            cell = function(value) {
              formatted_value <- if (curr == "MWK") {
                scales::comma(round(value, 0))
              } else {
                scales::dollar(value, digits = 2, prefix = "") # Using scales::dollar for proper decimal formatting
              }
              paste(symbol, formatted_value)
            },
            style = make_style_fn(price_range)
          ),
          `N. fishers` = reactable::colDef(
            format = reactable::colFormat(digits = 1),
            style = make_style_fn(fishers_range)
          ),
          `Trip length` = reactable::colDef(
            format = reactable::colFormat(digits = 1),
            cell = function(value) paste(round(value, 1), "hrs"),
            style = make_style_fn(trip_range)
          )
        )
      )
    })
  })
}
