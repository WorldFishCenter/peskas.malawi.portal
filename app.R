# Launch the ShinyApp (Do not remove this comment)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on top of this file

# Set production mode
options("golem.app.prod" = TRUE)

# Ensure R_CONFIG_ACTIVE is set if not already present in environment
if (Sys.getenv("R_CONFIG_ACTIVE") == "") {
  Sys.setenv(R_CONFIG_ACTIVE = "production")
}

# Initialize the application
peskas.malawi.portal::start_fun()

# Launch the application
peskas.malawi.portal::run_app()
