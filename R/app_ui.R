#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
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
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "peskas.malawi.portal"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
