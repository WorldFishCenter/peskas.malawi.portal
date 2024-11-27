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
    # Load configuration based on environment
    config <- config::get(file = "inst/golem-config.yml")

    # Validate critical environment variables in production
    if (Sys.getenv("R_CONFIG_ACTIVE") == "production") {
      required_vars <- c(
        "VAL_USER", "VAL_PASS", "MAPBOX_TOKEN",
        "KOBO_TOKEN", "ASSET_ID", "GCP_SA_KEY"
      )

      missing_vars <- required_vars[vapply(required_vars, function(x) {
        val <- Sys.getenv(x)
        is.null(val) || val == ""
      }, logical(1))]

      if (length(missing_vars) > 0) {
        warning("Missing required environment variables: ",
                paste(missing_vars, collapse = ", "))
      }

      # Ensure validation credentials are properly set
      config$validation$user <- Sys.getenv("VAL_USER", "")
      config$validation$pass <- Sys.getenv("VAL_PASS", "")
    }

    .app_env$pars <- config
  }
  invisible(.app_env$pars)
}

#' Get application parameters with fallback
#' @keywords internal
#' @noRd
get_pars <- function() {
  pars <- .app_env$pars
  if (is.null(pars)) {
    # Reload configuration if not initialized
    pars <- start_fun()
  }
  pars
}
