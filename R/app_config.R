#' Access files in the current app
#'
#' @param ... character vectors, specifying subdirectory and file(s)
#' within your package.
#'
#' @noRd
app_sys <- function(...) {
  if (Sys.getenv("R_CONFIG_ACTIVE") == "production") {
    # In production (Cloud Run), use the direct path
    file.path("/srv/shiny-server", ...)
  } else {
    # In development, use system.file
    system.file(..., package = "peskas.malawi.portal")
  }
}

#' Read App Config
#'
#' @param value Value to retrieve from the config file.
#' @param config GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE.
#' If unset, "default".
#' @param use_parent Logical, scan the parent directory for config file.
#' @param file Location of the config file
#'
#' @noRd
get_golem_config <- function(
    value,
    config = Sys.getenv(
      "GOLEM_CONFIG_ACTIVE",
      Sys.getenv(
        "R_CONFIG_ACTIVE",
        "default"
      )
    ),
    use_parent = TRUE,
    # Changed from inst/golem-config.yml to just golem-config.yml
    file = app_sys("golem-config.yml")) {

  # Add debug logging
  message("Loading config from: ", file)
  message("Config environment: ", config)

  if (!file.exists(file)) {
    warning("Config file not found at: ", file)
    # Try alternative locations
    alt_files <- c(
      "/srv/shiny-server/golem-config.yml",
      "/srv/shiny-server/inst/golem-config.yml"
    )

    for (alt_file in alt_files) {
      if (file.exists(alt_file)) {
        message("Using alternative config file: ", alt_file)
        file <- alt_file
        break
      }
    }
  }

  config::get(
    value = value,
    config = config,
    file = file,
    use_parent = use_parent
  )
}
