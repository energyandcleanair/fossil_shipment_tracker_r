library(shiny)
library(shinydashboard)

library(shinyBS)
library(leaflet)
library(plotly)

ui <- navbarPage(
    title=div(img(src="crea_logo.svg",
                  height=44)),
    windowTitle="CREA",
    theme = "theme.css",
    id = "nav-page",

    meta() %>%
      meta_social(
        title = "Russian Fossil Cost Counter",
        description = "Payments to Russia for fossil fuels by European Union since the invasion of Ukraine",
        url = "https://crea.shinyapps.io/russia_counter/",
        image = "https://energyandcleanair.org/wp/wp-content/uploads/2022/03/russia_counter_thumbnail.jpg",
        image_alt = "Russia Fossil Counter",
        twitter_creator = "@HubertThieriot",
        twitter_card_type = "summary",
        twitter_site = "@CREACleanAir"
      ),

    source(file.path("ui", "tab_counter.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_flows.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_methodology.R"),  local = TRUE)$value,
    source(file.path("ui", "tab_about.R"),  local = TRUE)$value,

    tags$head(
      tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/clipboard.js/2.0.4/clipboard.min.js")
    )
)


