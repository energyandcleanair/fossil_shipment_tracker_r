library(DT)
library(shinyWidgets)
library(shinycssloaders)
library(countrycode)
library(dplyr)
library(RColorBrewer)
library(rcrea)
library(colorRamps)
library(readr)

# library(shinyjs)

#####################
# Global variables
#####################
library(russiacounter)

environment <- "production"
base_url <- list("development"="https://development-dot-fossil-shipment-tracker.ew.r.appspot.com",
                 "production"="https://api.russiafossiltracker.com")[[environment]]


flows_sources <- c("Eurostat"="eurostat")
color_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
group_bys <- c("Country"="country", "Sector"="sector", "Fuel"="fuel")
selected_countries <- c("EU", "Germany", "Italy", "France", "Finland", "United Kingdom", "Austria", "Sweden")
# chart_types <- c("Bar (Horizontal)"="barh",
#                  "Bar (Vertical)"="barv",
#                  "Lines"="line",
#                  "Areas"="area")
chart_types <- c("Bar (Horizontal)"="barh")
topn <- 20 # How many rows max in chart
date_from_counter <- "2022-02-24"
sec_per_cycle <- 1
