
update_counter_new <- function(){

  library(tidyverse)
  library(lubridate)
  library(magrittr)
  library(countrycode)


  # Europe pipeline gas ------------------------------------------------------------

  # Pipeline gas to Europe
  flows_entsog <- entsog_new.get_flows(date_from='2020-01-01', use_cache=F)

  flows_entsog <- flows_entsog %>%
    mutate(departure_iso2=countrycode::countrycode(from_country, "country.name", "iso2c"),
           destination_iso2=countrycode::countrycode(to_country, "country.name", "iso2c")) %>%
    select(date, departure_iso2, destination_iso2, value_m3, value_mwh, value_tonne) %>%
    mutate(departure_iso2=ifelse(departure_country=='LNG', 'lng', departure_iso2)) %>%
    mutate(commodity='natural_gas')

  # saveRDS(flows_entsog, "cache/tmp_flows_entsog_new.RDS")
  # flows_entsog = readRDS("cache/tmp_flows_entsog_new.RDS")
  ok <- T
  # ok <- ok & (sum(flows_entsog$value_tonne) >= 2E8)
  # ok <- ok & all(flows_entsog$value_tonne >= -1)

  if(ok){
    db.upload_flows_to_postgres(flows_entsog, production=F)
    db.upload_flows_to_postgres(flows_entsog, production=T)
  }

  # Other European overland flows ----------------------------------------------------
  flows_overland_eu <- overland_eu.get_flows()
  db.upload_flows_to_postgres(flows_overland_eu, production=F)
  db.upload_flows_to_postgres(flows_overland_eu, production=T)

  # China --------------------------------------------------------
  flows_china <- china.get_flows() %>%
    mutate(value_mwh=NA)
  db.upload_flows_to_postgres(flows_china, production=F)
  db.upload_flows_to_postgres(flows_china, production=T)

  # Turkey --------------------------------------------------------
  flows_turkey <- turkey.get_flows() %>%
    mutate(value_mwh=NA)
  db.upload_flows_to_postgres(flows_turkey, production=F)
  db.upload_flows_to_postgres(flows_turkey, production=T)


  # V2: build and update prices to postgres
  price.update_prices(production=T)
  price.update_portprices(production=T)


  # # Ask platform to update counter
  # library(httr)
  # httr::POST("https://api.russiafossiltracker.com/v0/counter_update")
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
