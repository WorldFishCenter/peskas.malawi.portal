#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
    onStart = NULL,
    options = list(),
    enableBookmarking = NULL,
    uiPattern = "/",
    ...) {

  start_fun()

  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

#' Application parameters environment
#' @keywords internal
.app_env <- new.env(parent = emptyenv())

#' Shiny Application starting function
#'
#' @param global_pars Set global parameters
#' @return Invisibly returns the loaded parameters
#' @export
start_fun <- function(global_pars = TRUE) {
  if (isTRUE(global_pars)) {
    tryCatch({
      # Load configuration based on environment
      .app_env$pars <- config::get(file = system.file("golem-config.yml", package = "peskas.malawi.portal"))

      # In production, override with environment variables
      if (Sys.getenv("R_CONFIG_ACTIVE") == "production") {
        .app_env$pars$mapbox_token <- Sys.getenv("MAPBOX_TOKEN")
        # Log for debugging
        message("Production environment detected")
        message("Mapbox token set: ", nzchar(Sys.getenv("MAPBOX_TOKEN")))
      }
    }, error = function(e) {
      warning("Error loading configuration: ", e$message)
      return(NULL)
    })
  }
  invisible(.app_env$pars)
}

#' Get application parameters with fallback
#' @keywords internal
#' @noRd
get_pars <- function() {
  pars <- .app_env$pars
  if (is.null(pars)) {
    message("Parameters not initialized, attempting to reload...")
    pars <- start_fun()
  }
  if (is.null(pars$mapbox_token)) {
    warning("Mapbox token not found in configuration")
  }
  pars
}
