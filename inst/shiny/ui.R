library(shiny)
library(shinydashboard)
library(metathis)
library(shinyBS)
library(leaflet)
library(plotly)

library(shiny)
library(shinydashboard)

library(shinyBS)
library(leaflet)
library(plotly)

ui <- fluidPage(
  windowTitle="Russia Ïossil Counter",
  theme = "theme.css",
  id = "counter",

  source(file.path("ui", "section_header.R"),  local = TRUE)$value,
  source(file.path("ui", "section_counter.R"),  local = TRUE)$value,
  # source(file.path("ui", "section_maintenance.R"),  local = TRUE)$value,
  source(file.path("ui", "section_briefing.R"),  local = TRUE)$value,
  source(file.path("ui", "section_payments.R"),  local = TRUE)$value,
  source(file.path("ui", "section_monthly.R"),  local = TRUE)$value,
  source(file.path("ui", "section_mailchimp.R"),  local = TRUE)$value,
  source(file.path("ui", "section_countries.R"),  local = TRUE)$value,
  # source(file.path("ui", "section_diagnostic.R"),  local = TRUE)$value,
  source(file.path("ui", "section_methodology.R"),  local = TRUE)$value,
  source(file.path("ui", "section_about.R"),  local = TRUE)$value
)


