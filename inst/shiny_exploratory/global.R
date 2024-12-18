library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(countrycode)
library(dplyr)
library(shinyURL)
library(rclipboard)
library(russiacounter)
library(tidyverse)
library(RColorBrewer)
library(rcrea)
library(colorRamps)
library(readr)
library(DT)
library(leaflet)
library(leaflet.extras2)
library(sf)
library(geojsonsf)
library(lubridate)


#####################
# Global variables
#####################

environment <- "production"
base_url <- list("development"="https://development-dot-fossil-shipment-tracker.ew.r.appspot.com",
                 "production"="https://api.russiafossiltracker.com")[[environment]]


frequency <- c(
  # "Hourly"="hour",
  "Daily"="day",
  "Weekly"="week",
  "Monthly"="month",
  "Yearly"="year")

units <- c("eur","tonne")

data_type <- c("Total (Shipments + Pipeline)" = "counter",
               "Shipments" = "shipments")

plot_types <- c("Area" = "area",
                "Area (share)" = "area_pct",
                "Lines" = "lines",
                "Bars" = "bar")

colour_bys <- c("Region" = "destination_region",
              "Country" = "destination_country",
              "Commodity" = "commodity",
              "Commodity group" = "commodity_group")

colour_bys_shipments <- c("Region" = "destination_region",
                "Country" = "destination_country",
                "Commodity" = "commodity",
                "Commodity group" = "commodity_group",
                "Port" = "port_name",
                "Buying company" = "arrival_berth_owner",
                "Ship insurer" = "ship_insurer",
                "Ship manager" = "ship_manager",
                "Ship owner" = "ship_owner")

group_bys <- c("Region" = "destination_region",
              "Country" = "destination_country",
              "Commodity" = "commodity",
              "Commodity group" = "commodity_group")

group_bys_shipments <- c("Region" = "destination_region",
                          "Country" = "destination_country",
                          "Commodity" = "commodity",
                          "Commodity group" = "commodity_group",
                          "Port" = "port_name",
                          "Ship insurer" = "ship_insurer",
                          "Ship manager" = "ship_manager",
                          "Ship owner" = "ship_owner")

commodity_palette <- rcrea::pal_crea.dramatic

presets <- c(
  "Custom" = "custom",
  "Wind & solar contribution" = "windsolarmonth")

preset_params <- list(
  "windsolarmonth" = list(
    "frequency"="month",
    "sources"=c("Wind","Solar","Renewables"), #Renewable and Wind|Solar are exclusive
    "plot_type"="bar"
    # "date_from"="2016-01-01",
    # "date_to"=lubridate::today()
  )
)


# Palettes
palettes <- list(
  commodity_group = list(coal="#351c75",
                gas="#f6b26b",
                oil="#741b47"))
