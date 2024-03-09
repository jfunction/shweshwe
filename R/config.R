getConfig <- function(localProfile = "localDbg") {
  # Bring in the configuration profile
  R_CONFIG_ACTIVE = Sys.getenv("R_CONFIG_ACTIVE")
  if (R_CONFIG_ACTIVE!="shinyapps") {
    R_CONFIG_ACTIVE = localProfile
    # See config.yml for settings
  }
  Sys.setenv(R_CONFIG_ACTIVE = R_CONFIG_ACTIVE)
  print(paste("R_CONFIG_ACTIVE=", R_CONFIG_ACTIVE, sep=""))
  config::get()
}
