
update_counter <- function(){

  library(tidyverse)
  library(lubridate)
  library(magrittr)
  library(countrycode)


  # Europe pipeline gas ------------------------------------------------------------

  # Pipeline gas to Europe
  print("=== ENTSOG ===")
  flows_entsog <- entsog_new.get_flows(date_from=lubridate::today()-10, use_cache=F)
  ok <- T
  ok <- ok & (sum(flows_entsog$value_tonne) >= as.integer(max(flows_entsog$date)-min(flows_entsog$date)) * 5e5)
  # ok <- ok & all(flows_entsog$value_tonne >= -1)

  if(ok){
    # db.upload_flows_to_postgres(flows_entsog, production=F)
    db.upload_flows_to_postgres(flows_entsog, production=T)
  }

  # Other European overland flows ----------------------------------------------------
  print("=== Overland EU ===")
  flows_overland_eu <- overland_eu.get_flows()
  # db.upload_flows_to_postgres(flows_overland_eu, production=F)
  db.upload_flows_to_postgres(flows_overland_eu, production=T)

  # China --------------------------------------------------------
  print("=== China ===")
  flows_china <- china.get_flows() %>%
    mutate(value_mwh=NA_real_)
  # db.upload_flows_to_postgres(flows_china, production=F)
  db.upload_flows_to_postgres(flows_china, production=T)

  # Turkey --------------------------------------------------------
  print("=== Turkey ===")
  flows_turkey <- turkey.get_flows() %>%
    mutate(value_mwh=NA_real_)
  # db.upload_flows_to_postgres(flows_turkey, production=F)
  db.upload_flows_to_postgres(flows_turkey, production=T)


  # Prices ------------------------------------------------------------------
  print("=== Prices ===")
  price.update_prices(production=T)
  price.update_portprices(production=T)


  # # Ask platform to update counter
  print("=== Counter ===")
  library(httr)
  httr::POST("https://api.russiafossiltracker.com/v0/counter_update")
  #
  #
  # # Collect data
  # counter_last <- read_csv("https://api.russiafossiltracker.com/v0/counter_last?destination_region=EU28&aggregate_by=destination_region,commodity_group&format=csv")
  # counter_last_new <- counter_last %>% select(date, commodity_group, total_eur, eur_per_day) %>%
  #   tidyr::pivot_wider(values_from=c(total_eur, eur_per_day), names_from="commodity_group") %>%
  #   rename(cumulated_coal_eur=total_eur_coal,
  #          cumulated_gas_eur=total_eur_gas,
  #          cumulated_oil_eur=total_eur_oil,
  #          cumulated_total_eur=total_eur_total,
  #          coal_eur=eur_per_day_coal,
  #          gas_eur=eur_per_day_gas,
  #          oil_eur=eur_per_day_oil,
  #          total_eur=eur_per_day_total
  #          )
  #
  # # Upload (it only updates the counter progressively)
  # upload_counter_data(counter_last_new)
}
