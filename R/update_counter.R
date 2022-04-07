
update_counter <- function(){

  library(tidyverse)
  library(lubridate)
  library(magrittr)


  # Only ENTSOG needs to be updated regularly for now
  update_flows(source="entsog")

  # update_flows(source="comtrade_eurostat", use_cache=T)
  # update_flows(source="eurostat", use_cache=T, date_from="2019-01-01")
  # update_flows(source="comtrade", use_cache=F)
  # update_flows(source="eurostat_exeu", use_cache=F)
  # update_flows(source="eurostat_byhs", use_cache=F)


  eurostat.get_flows(use_cache=F)
  comtrade.get_flows(use_cache=F)
  eurostat_exeu.get_flows(use_cache=F)
  comtrade_eurostat.get_flows(use_cache=F)



  # Computing prices --------------------------------------------------------
  flows_entsog = db.download_flows("entsog")
  flows_eurostat_exeu = db.download_flows("eurostat_exeu")
  flows_comtrade_eurostat = db.download_flows("comtrade_eurostat")
  flows_comtrade_eurostat_2022 = utils.expand_in_2022(flows_comtrade_eurostat, flows_eurostat_exeu)

  prices <- price.get_modelled_price(flows_entsog=flows_entsog,
                                     flows_comtrade_eurostat=flows_comtrade_eurostat_2022)


  # Uploading information in the database -----------------------------------
  db.upload_flows(flows=prices, source="combined")
  prices_light <- prices %>%
    filter(date>="2021-01-01") %>%
    group_by(date, source, commodity, transport, unit) %>%
    summarise_at(c("value","value_eur"), sum, na.rm=T) %>%
    filter(!is.na(source))

  db.upload_flows(flows=prices_light, source="combined_light")

  # Counter data, the one that is queried by the API
  if(!all(prices$country=="EU" | prices$commodity=="natural_gas")){
    stop("Prices doesn't have proper countries")
  }

  counter_data <- prices %>%
    ungroup() %>%
    filter(date>=date_from_counter) %>%
    filter(date<=lubridate::today()) %>%
    group_by(date, commodity) %>%
    summarise(value_eur=sum(value_eur, na.rm=T)) %>%
    tidyr::pivot_wider(names_from=commodity,values_from=value_eur) %>%
    # Take lastest day with natural_gas (pipeline) data
    filter(!is.na(natural_gas)) %>%
    mutate(gas_eur=natural_gas+lng,
           oil_eur=oil + oil_others,
           coal_eur=coal+coke,
           total_eur=coal_eur + oil_eur + gas_eur) %>%
    select(date, coal_eur, gas_eur, oil_eur, total_eur) %>%
    arrange(date) %>%
    ungroup() %>%
    mutate(across(c(coal_eur, gas_eur, oil_eur, total_eur), cumsum, .names='cumulated_{.col}')) %>%
    filter(!is.na(total_eur))

  # Upload if nothing is strange
  ok <- !any(is.na(counter_data))
  ok <- ok & all(c("date",
                   "coal_eur", "gas_eur", "oil_eur", "total_eur",
                   "cumulated_coal_eur", "cumulated_gas_eur", "cumulated_oil_eur", "cumulated_total_eur") %in% names(counter_data))
  ok <- ok & nrow(counter_data>0)

  if(ok){
    print("Updating counter data")
    db.update_counter(counter_data)
  }
}
