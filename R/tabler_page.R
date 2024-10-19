tabler_page <- function(..., dark = FALSE, title = NULL, favicon = NULL) {
  add_resource_path("www", app_sys("app/www"), T)

  # Set the theme attribute based on the dark argument
  theme_data_attribute <- if (dark) "dark" else "light"

  args <- list(
    tags$head(
      tags$meta(
        name = "viewport",
        content = "width=device-width, initial-scale=1"
      ),
      tags$meta(charset = "utf-8"),
      tags$title(title),
      tags$link(
        rel = "stylesheet",
        href = "https://unpkg.com/@tabler/core@1.0.0-beta21/dist/css/tabler.min.css"
      ),
      tags$link(
        rel = "stylesheet",
        href = "www/tabler.css"
      )
    ),
    tags$div(
      class = "wrapper",
      `data-bs-theme` = theme_data_attribute,  # Apply the data-bs-theme attribute
      ...
    ),
    tags$script(
      src = "https://unpkg.com/@tabler/core@1.0.0-beta21/dist/js/tabler.min.js",
      defer = "defer"
    )
  )

  do.call(tagList, args)
}

page_heading <- function(pretitle = "Page pretitle", title = "Page title", ...) {
  tags$div(
    class = "page-header d-print-none",
    tags$div(
      class = "container-xl",
      tags$div(
        class = "row g-2 align-items-center",
        tags$div(
          class = "col",
          tags$div(
            class = "page-pretitle",
            pretitle
          ),
          tags$h2(
            class = "page-title",
            title
          )
        ),
        ...
      )
    )
  )
}
