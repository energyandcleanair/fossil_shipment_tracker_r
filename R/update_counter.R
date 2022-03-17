
update_counter <- function(){

  library(tidyverse)
  library(lubridate)
  library(magrittr)

  # update_flows(source="eurostat_exeu") # No need, always the same?
  update_flows(source="entsog")
  # update_flows(source="eurostat", use_cache=T, date_from="2019-01-01")
  # update_flows(source="comtrade", use_cache=F)
  update_flows(source="eurostat_exeu", use_cache=T)
  # update_flows(source="eurostat_byhs", use_cache=F)
  update_flows(source="comtrade_eurostat", use_cache=T)

  flows_eurostat_exeu = eurostat_exeu.get_flows(use_cache=T)
  flows_comtrade_eurostat = comtrade_eurostat.get_flows(use_cache=T)
  flows_comtrade_eurostat_2022 = utils.expand_in_2022(flows_comtrade_eurostat, flows_eurostat_exeu)

  prices <- price.get_modelled_price(flows_entsog=entsog.get_flows(use_cache=T),
                                     flows_comtrade_eurostat=flows_comtrade_eurostat_2022)

  # prices <- prices %>% filter(transport %in% c("pipeline","sea"))

  db.upload_flows(flows=prices, source="combined")



  # For faster loading
  prices_light <- prices %>%
    filter(date>="2021-01-01") %>%
    group_by(date, source, commodity, transport, unit) %>%
    summarise_at(c("value","value_eur"), sum, na.rm=T) %>%
    filter(!is.na(source))

  db.upload_flows(flows=prices_light, source="combined_light")

  # For even faster loading: prepare counter data beforehand,
  # Will make it available in API
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
    mutate(across(c(coal_eur, gas_eur, oil_eur, total_eur), cumsum, .names='cumulated_{.col}'))

  db.update_counter(counter_data)

}
