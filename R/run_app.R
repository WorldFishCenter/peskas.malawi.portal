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
    .app_env$pars <- config::get(file = "inst/golem-config.yml")
  }
  invisible(.app_env$pars)
}

#' Get application parameters
#' @keywords internal
#' @noRd
get_pars <- function() {
  .app_env$pars
}
