
update_counter <- function(){

  library(tidyverse)
  library(lubridate)
  library(magrittr)
  library(countrycode)
  library(rcrea)
  library(pbapply)
  library(eurostat)


  # Europe pipeline gas ------------------------------------------------------------

  # Pipeline gas to Europe
  flows_entsog <- entsog_new.get_flows(date_from=lubridate::today()-21, use_cache=F)

  ok <- T
  ok <- ok & (sum(flows_entsog$value_tonne) >= as.integer(max(flows_entsog$date)-min(flows_entsog$date)) * 5e5)
  if(ok){
    db.upload_flows_to_postgres(flows_entsog, production=T)
  }

  # Other European overland flows ----------------------------------------------------
  flows_overland_eu <- overland_eu.get_flows()
  # db.upload_flows_to_postgres(flows_overland_eu, production=F)
  db.upload_flows_to_postgres(flows_overland_eu, production=T)

  # China --------------------------------------------------------
  flows_china <- china.get_flows() %>%
    mutate(value_mwh=NA_real_)
  # db.upload_flows_to_postgres(flows_china, production=F)
  db.upload_flows_to_postgres(flows_china, production=T)

  # Turkey --------------------------------------------------------
  flows_turkey <- turkey.get_flows() %>%
    mutate(value_mwh=NA_real_)
  # db.upload_flows_to_postgres(flows_turkey, production=F)
  db.upload_flows_to_postgres(flows_turkey, production=T)


  # V2: build and update prices to postgres
  price.update_prices(production=T)

}
