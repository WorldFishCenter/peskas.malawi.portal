#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Load dependencies in specific order
    tags$head(
      # Core dependencies first
      tags$script(src = "www/vendor/jquery.min.js"),

      # Then UI framework
      tags$link(rel = "stylesheet", href = "www/vendor/tabler.min.css"),
      tags$script(src = "www/vendor/tabler.min.js", defer = FALSE), # Remove defer to ensure it loads before others

      # Then visualization libraries
      tags$script(src = "www/vendor/apexcharts.min.js"),
      tags$script(src = "www/vendor/deck.min.js"),

      # Add loading check
      tags$script("
        window.addEventListener('load', function() {
          if (typeof ApexCharts === 'undefined' ||
              typeof deck === 'undefined') {
            console.error('Required libraries not loaded');
            location.reload();
          }
        });
      ")
    ),
    # Your application UI logic
    tabler_page(
      title = "Peskas | Malawi",
      header(
        logo = peskas_logo(),
        version_flex(
          heading = "Management Dashboard",
          subheading = "Malawi (0.0.0.9000 - beta)"
        )
      ),
      tab_menu(
        tab_menu_item(
          label = tagList(
            "Home",
          ),
          id = "home", icon_home()
        ),
        tab_menu_item(
          label = tagList(
            "Catch"
          ),
          id = "catch", icon_scale()
        ),
        tab_menu_item(
          label = tagList(
            "Revenue"
          ),
          id = "revenue", icon_currency_dollar()
        )
      ),
      tabset_panel(
        menu_id = "main_tabset",
        tab_panel(
          id = "home",
          tab_home_content()
        ),
        tab_panel(
          id = "catch",
          tab_catch_content()
        ),
        tab_panel(
          id = "revenue",
          tab_revenue_content()
        )
      ),
      footer_panel(
        left_side_elements = tags$li(
          class = "list-inline-item",
          "Last update",
        ),
        right_side_elements = tagList(
          inline_li_link(
            content = "Licence",
            href = "https://github.com/WorldFishCenter/peskas.malawi.portal/blob/main/LICENSE.md"
          ),
          inline_li_link(
            content = "Code",
            href = "https://github.com/WorldFishCenter/peskas.malawi.portal"
          )
        ),
        bottom = "Copyright \u00a9 2024 Peskas. All rights reserved."
      )
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @noRd
golem_add_external_resources <- function() {
  shiny::addResourcePath(
    "www", app_sys("app/www")
  )

  tags$head(
    # avoiding some overhead so that we don't need to use golem in production
    # favicon(),
    # bundle_resources(
    # path = app_sys('app/www'),
    # app_title = 'peskas.timor.portal'
    # )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}

#' Remove the old dependency functions since we're using local files now
apexchart_dep <- function() {
  htmltools::htmlDependency(
    name = "apexcharts",
    version = "3.26.2",
    src = "www/vendor",
    script = "apexcharts.min.js"
  )
}

jquery_dep <- function() {
  htmltools::htmlDependency(
    name = "jquery",
    version = "3.6.0",
    src = "www/vendor",
    script = "jquery.min.js"
  )
}

deckgl_dep <- function() {
  htmltools::htmlDependency(
    name = "deckgl",
    version = "9.0.34",
    src = "www/vendor",
    script = "deck.min.js"
  )
}
