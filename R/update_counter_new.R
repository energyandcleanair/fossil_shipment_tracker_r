
update_counter_new <- function(){

  library(tidyverse)
  library(lubridate)
  library(magrittr)


  # Europe pipeline gas ------------------------------------------------------------

  # Pipeline gas to Europe
  flows_entsog <- entsog.get_flows(use_cache=F) %>%
    mutate(departure_iso2=countrycode::countrycode(partner, "country.name", "iso2c"),
           destination_iso2=countrycode::countrycode(country, "country.name", "iso2c")) %>%
    tidyr::pivot_wider(values_from="value", names_from="unit") %>%
    rename(value_mwh=`MWh/day`) %>%
    mutate(value_tonne=value_mwh/gcv_MWh_per_m3*kg_per_m3/1000) %>%
    mutate(value_m3=value_mwh/gcv_MWh_per_m3) %>%
    select(date, departure_iso2, destination_iso2, value_mwh, value_tonne) %>%
    mutate(departure_iso2="RU") %>% # Assuming Belarus, Ukraine are just transit
    mutate(commodity='natural_gas')


  # Distribute to country
  # distrib_entsog <- utils.get_russia_pipeline_distribution() %>%
  #   arrange(desc(share_of_russia))

  # Compare with Lauri's
  pipeline_share <- utils.get_transport_share() %>%
    filter(commodity=="gas_all",
           transport=="pipeline")

  distrib_entsog_lauri <- read_csv("data/EU fossil fuel imports from RU, by country, bln EUR 2019.csv") %>%
    select(destination_country=reporter, value = Gas) %>%
    mutate(share_of_russia=value/sum(value)) %>%
    arrange(desc(share_of_russia)) %>%
    mutate(destination_iso2=countrycode(destination_country, "country.name", "iso2c"))

  sum(distrib_entsog_lauri$share_of_russia) == 1

  flows_entsog_distributed <- flows_entsog %>%
    group_by(date, departure_iso2, commodity) %>%
    summarise_at(c("value_mwh", "value_tonne"), sum, na.rm=T) %>%
    crossing(distrib_entsog_lauri) %>%
    mutate_at(c("value_mwh", "value_tonne"), function(x) x * .$share_of_russia) %>%
    mutate(value_m3=NA)

  sum(flows_entsog$value_tonne) == sum(flows_entsog_distributed$value_tonne)

  db.upload_flows_to_postgres(flows_entsog_distributed)

  # Other European overland flows ----------------------------------------------------
  trade_share <- utils.get_transport_share()
  #TODO missing coke

  flows_comtrade_eurostat = db.download_flows("comtrade_eurostat")
  flows_eurostat_exeu = db.download_flows("eurostat_exeu")
  flows_comtrade_eurostat_2022 = utils.expand_in_2022(flows_comtrade_eurostat, flows_eurostat_exeu)
  unique(flows_comtrade_eurostat_2022$commodity)

  flows_overland_non_pipeline_eu <- flows_comtrade_eurostat_2022 %>%
    filter(partner=="Russia") %>%
    mutate(departure_iso2="RU") %>%
    mutate(commodity=recode(commodity, oil_others="oil_products")) %>%
    filter(country!="EU", EU) %>%
    filter(unit=="tonne") %>%
    left_join(trade_share %>%
                filter(transport != "pipeline" | !grepl("gas", commodity)) %>%
                filter(transport != "seaborne") %>%
                filter(transport != "other") %>%
                group_by(country, destination_iso2=iso2, commodity, transport) %>%
                summarise(share=sum(share, na.rm=T))) %>%
    mutate(value=value*share) %>%
    select(departure_iso2, commodity, transport, destination_iso2, date, value_tonne=value) %>%
    filter(!is.na(transport)) %>%
    mutate(commodity=paste(commodity, transport, sep="_")) %>%
    mutate(commodity = recode(commodity,
                              oil_pipeline="pipeline_oil",
                              oil_rail_road="crude_oil_rail_road")) %>%
    filter(value_tonne>0) %>%
    filter(!is.infinite(value_tonne)) %>%
    mutate(value_m3=NA, value_mwh=NA)

  db.upload_flows_to_postgres(flows_overland_non_pipeline_eu)

  # China --------------------------------------------------------
  flows_china <- china.get_flows() %>%
    mutate(value_mwh=NA)
  db.upload_flows_to_postgres(flows_china)

  # Turkey --------------------------------------------------------
  flows_turkey <- turkey.get_flows() %>%
    mutate(value_mwh=NA)
  db.upload_flows_to_postgres(flows_turkey)


  # V2: build and update prices to postgres
  price.update_prices()
  price.update_portprices()


  # Ask platform to update counter
  library(httr)
  httr::POST("https://api.russiafossiltracker.com/v0/counter_update")


  # Collect data
  counter_last <- read_csv("https://api.russiafossiltracker.com/v0/counter_last?destination_region=EU28&aggregate_by=destination_region,commodity_group&format=csv")
  counter_last_new <- counter_last %>% select(date, commodity_group, total_eur, eur_per_day) %>%
    tidyr::pivot_wider(values_from=c(total_eur, eur_per_day), names_from="commodity_group") %>%
    rename(cumulated_coal_eur=total_eur_coal,
           cumulated_gas_eur=total_eur_gas,
           cumulated_oil_eur=total_eur_oil,
           cumulated_total_eur=total_eur_total,
           coal_eur=eur_per_day_coal,
           gas_eur=eur_per_day_gas,
           oil_eur=eur_per_day_oil,
           total_eur=eur_per_day_total
           )

  # Upload (it only updates the counter progressively)
  upload_counter_data(counter_last_new)

}
