# Launch the ShinyApp (Do not remove this comment)
options("golem.app.prod" = TRUE)

# Ensure R_CONFIG_ACTIVE is set
if (Sys.getenv("R_CONFIG_ACTIVE") == "") {
  Sys.setenv(R_CONFIG_ACTIVE = "production")
}

# Initialize parameters explicitly
tryCatch({
  params <- peskas.malawi.portal::start_fun()
  # Add debug logging
  message("Mapbox token available: ", !is.null(params$mapbox_token))
  message("Config environment: ", Sys.getenv("R_CONFIG_ACTIVE"))
}, error = function(e) {
  message("Error initializing parameters: ", e$message)
})

peskas.malawi.portal::run_app()
