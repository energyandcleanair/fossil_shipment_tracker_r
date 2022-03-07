
#' @export
deployShinyApp <- function() {
  if(!require(rsconnect)) install.packages('reconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  # # Basically telling ShinyApps where to get creahia
  # urls <- c(
  #   # "trafficonese/leaflet.extras2",
  #   "energyandcleanair/creahelpers",
  #   "energyandcleanair/rcrea")
  # remotes::install_github(urls, force=T, upgrade="never")

  # try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))
  # # Deploy production
  rsconnect::deployApp("inst/shiny",
                       appName="russia_counter",
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = T)

}
