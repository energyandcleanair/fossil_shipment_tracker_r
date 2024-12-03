update_counter <- function(rebuild_prices = F) {
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(magrittr))
  suppressPackageStartupMessages(library(countrycode))
  suppressPackageStartupMessages(library(rcrea))
  suppressPackageStartupMessages(library(pbapply))
  suppressPackageStartupMessages(library(eurostat))
  suppressPackageStartupMessages(library(forecast))

  log_level(STAGE, "Updating prices")
  price.update_prices(production = T, buffer_days = 60, rebuild = rebuild_prices)

  log_level(STAGE, "Updating european pipeline gas")
  flows_entsog <- entsog_new.get_flows(date_from = lubridate::today() - 21, use_cache = F) %>%
    mutate(
      initial_origin_insertion_method = "pipeline",
      process = "gas_model"
    )
  ok <- T & (sum(flows_entsog$value_tonne) >= as.integer(max(flows_entsog$date) - min(flows_entsog$date)) * 5e5)
  if (ok) {
    db.upload_flows_to_postgres(flows_entsog, production = T)
  }

  log_level(STAGE, "Updating other european overland flows")
  flows_overland_eu <- overland_eu.get_flows() %>%
    mutate(
      initial_origin_insertion_method = NA,
      process = "overland_eu"
    )
  db.upload_flows_to_postgres(flows_overland_eu, production = T)

  log_level(STAGE, "Updating China flows")
  flows_china <- china.get_flows() %>%
    mutate(
      value_mwh = NA_real_,
      initial_origin_insertion_method = NA,
      process = "china"
    )
  db.upload_flows_to_postgres(flows_china, production = T)

  log_level(STAGE, "Updating India flows")
  flows_turkey <- turkey.get_flows() %>%
    mutate(
      value_mwh = NA_real_,
      initial_origin_insertion_method = NA,
      process = "turkey"
    )
  db.upload_flows_to_postgres(flows_turkey, production = T)
}
