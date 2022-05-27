
#' @export
deployShinyApp <- function(version, test=T) {
  # Version = lite, standard, exploratory

  if(!require(rsconnect)) install.packages('reconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  # Telling Shinyapps where to find packages
  urls <- c(
    "energyandcleanair/202203_russian_gas",
    "energyandcleanair/rcrea",
    "energyandcleanair/entsog")
  remotes::install_github(urls, force=T, upgrade="never")

  # try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))

  options(rsconnect.http.trace = F)
  options(rsconnect.http.verbose = F)

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))


  folder <- list(lite="inst/shiny_lite",
                 standard="inst/shiny",
                 exploratory="inst/shiny_exploratory")[[version]]

  name <- list(lite="russia_counter_lite",
                 standard="russia_counter",
                 exploratory="russia_counter_exploratory")[[version]]

  rsconnect::deployApp(folder,
                       appName=paste0(name,ifelse(test,"_test","")),
                       account = Sys.getenv("SHINYAPP_ACCOUNT"),
                       forceUpdate = T)
}
