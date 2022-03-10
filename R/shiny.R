
#' @export
deployShinyApp <- function(lite=T, test=T) {
  if(!require(rsconnect)) install.packages('reconnect')
  if(!require(dotenv)) install.packages('dotenv')
  if(!require(devtools)) install.packages('devtools')

  # Telling Shinyapps where to find packages
  urls <- c(
    "energyandcleanair/202203_russian_gas",
    "energyandcleanair/rcrea")
  remotes::install_github(urls, force=T, upgrade="never")

  # try(dotenv::load_dot_env())
  try(readRenviron(".Renviron"))

  rsconnect::setAccountInfo(name=Sys.getenv("SHINYAPP_ACCOUNT"),
                            token=Sys.getenv("SHINYAPP_TOKEN"),
                            secret=Sys.getenv("SHINYAPP_SECRET"))
  # Two versions
  if(lite){
    rsconnect::deployApp("inst/shiny_lite",
                         appName=paste0("russia_counter_lite",ifelse(test,"_test","")),
                         account = Sys.getenv("SHINYAPP_ACCOUNT"),
                         forceUpdate = T)
  }else{
    rsconnect::deployApp("inst/shiny",
                         appName=paste0("russia_counter",ifelse(test,"_test","")),
                         account = Sys.getenv("SHINYAPP_ACCOUNT"),
                         forceUpdate = T)
  }
}
