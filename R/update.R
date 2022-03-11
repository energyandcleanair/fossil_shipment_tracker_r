update_counter <- function(){

  library(tidyverse)
  library(lubridate)


  # update_flows(source="eurostat_exeu") # No need, always the same
  update_flows(source="entsog")
  update_flows(source="eurostat", use_cache=T, date_from="2019-01-01")
  update_flows(source="comtrade", use_cache=F)
  update_flows(source="eurostat_exeu", use_cache=F)
  update_flows(source="eurostat_byhs", use_cache=F)
  update_flows(source="comtrade_eurostat_russia", use_cache=T)

  prices <- price.get_modelled_price(flows_entsog=entsog.get_flows(use_cache=T),
                                     flows_eurostat_exeu=eurostat_exeu.get_flows(use_cache=T))
  db.upload_flows(flows=prices, source="combined")

  # For faster loading
  prices_light <- prices %>%
    filter(date>="2022-01-01") %>%
    group_by(date, source, commodity, transport, unit) %>%
    summarise_at(c("value","value_eur"), sum, na.rm=T) %>%
    filter(!is.na(source))

  db.upload_flows(flows=prices_light, source="combined_light")
}
