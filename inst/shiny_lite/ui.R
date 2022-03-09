library(shiny)
library(shinydashboard)

library(shinyBS)
library(leaflet)
library(plotly)

ui <- fluidPage(
    windowTitle="Russia Ïossil Counter",
    theme = "theme.css",
    id = "counter",
    source(file.path("ui", "tab_counter.R"),  local = TRUE)$value
)


