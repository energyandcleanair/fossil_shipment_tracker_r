library(shiny)
library(shinydashboard)

library(shinyBS)
library(leaflet)
library(plotly)

ui <- navbarPage(
    title=div(img(src="crea_logo.svg", height=44)),
    windowTitle="CREA",
    theme = "theme.css",
    id = "nav-page",

    source(file.path("ui", "tab_main.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_voyages.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_diagnostic.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_about.R"),  local = TRUE)$value,

    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.4/clipboard.min.js")
    )
)


