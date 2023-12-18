update_counter <- function(rebuild_prices = F) {
  library(tidyverse)
  library(lubridate)
  library(magrittr)
  library(countrycode)
  library(rcrea)
  library(pbapply)
  library(eurostat)
  library(forecast)

  message("=== Updating counter ===")
  message("--- Updating prices ---")
  # Price should come first, as many endpoints rely on this to be complete
  # Run the line below after updating price models
  # price.update_prices(production=T, rebuild = T)
  price.update_prices(production = T, buffer_days = 60, rebuild = rebuild_prices)

  message("--- Updating european pipeline gas ---")
  flows_entsog <- entsog_new.get_flows(date_from = lubridate::today() - 21, use_cache = F)
  ok <- T
  ok <- ok & (sum(flows_entsog$value_tonne) >= as.integer(max(flows_entsog$date) - min(flows_entsog$date)) * 5e5)
  if (ok) {
    db.upload_flows_to_postgres(flows_entsog, production = T)
  }

  message("--- Updating other european overland flows ---")
  flows_overland_eu <- overland_eu.get_flows()
  db.upload_flows_to_postgres(flows_overland_eu, production = T)

  message("--- Updating China flows ---")
  flows_china <- china.get_flows() %>%
    mutate(value_mwh = NA_real_)
  db.upload_flows_to_postgres(flows_china, production = T)

  message("--- Updating India flows ---")
  flows_turkey <- turkey.get_flows() %>%
    mutate(value_mwh = NA_real_)
  db.upload_flows_to_postgres(flows_turkey, production = T)
}
