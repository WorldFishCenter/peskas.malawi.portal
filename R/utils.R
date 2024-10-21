#' Read configuration file
#'
#' Reads configuration file in `config.yml` and adds some logging lines. Wrapped
#' for convenience
#'
#' @return the environment parameters
#'
#' @keywords helper
#' @export
#'
read_config <- function() {
  pars <- config::get(
    config = Sys.getenv("R_CONFIG_ACTIVE", "default"),
    file = system.file("config.yml", package = "peskas.malawi.portal")
  )
  pars
}
