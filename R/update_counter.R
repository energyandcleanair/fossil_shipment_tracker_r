
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


  # eurostat.get_flows(use_cache=F)
  # comtrade.get_flows(use_cache=F)
  # eurostat_exeu.get_flows(use_cache=F)
  # comtrade_eurostat.get_flows(use_cache=F)



  # Computing prices --------------------------------------------------------
  flows_entsog = db.download_flows("entsog")
  flows_eurostat_exeu = db.download_flows("eurostat_exeu")
  flows_comtrade_eurostat = db.download_flows("comtrade_eurostat")
  flows_comtrade_eurostat_2022 = utils.expand_in_2022(flows_comtrade_eurostat, flows_eurostat_exeu)

  prices <- price.get_modelled_price(flows_entsog=flows_entsog,
                                     flows_comtrade_eurostat=flows_comtrade_eurostat_2022)

  # Uploading information in the database -----------------------------------
  db.upload_flows(flows=prices, source="combined")

  # A liter version, used for the price endpoint
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

  upload_counter_data(counter_data, prices)
}


upload_counter_data <- function(counter_data, prices=NULL){

  # Upload if nothing is strange
  ok <- !any(is.na(counter_data))
  ok <- !any(counter_data==0)
  ok <- ok & all(c("date",
                   "coal_eur", "gas_eur", "oil_eur", "total_eur",
                   "cumulated_coal_eur", "cumulated_gas_eur", "cumulated_oil_eur", "cumulated_total_eur") %in% names(counter_data))
  ok <- ok & nrow(counter_data>0)
  ok <- ok & nrow(counter_data) == max(counter_data$date) - min(counter_data$date) + 1



  # ONLY ALLOW FOR LIMITED UPDATE EACH TIME
  # SO THAT COUNTER DOESN"T JUMP TOO MUCH
  max_diff_per_update <- 200E6 # maximum gap in eur for total number

  update_til_now <- function(c, now){
    difftime_day <- as.numeric(difftime(lubridate::force_tz(as.POSIXct(now), tzone="UTC"), lubridate::force_tz(as.POSIXct(max(c$date)), tzone="UTC")), "days")
    c %>%
      filter(date==max(c$date)) %>%
      mutate(date = now,
             cumulated_coal_eur = cumulated_coal_eur + coal_eur * difftime_day,
             cumulated_oil_eur = cumulated_oil_eur + oil_eur * difftime_day,
             cumulated_gas_eur = cumulated_gas_eur + gas_eur * difftime_day,
             cumulated_total_eur = cumulated_total_eur + total_eur * difftime_day)
  }



  # But update progressively !
  now <- lubridate::now("UTC")
  counter_data_bkp <- db.download_counter()
  dir.create("bkp")
  saveRDS(counter_data_bkp, sprintf("bkp/counter_data_%s.RDS", strftime(lubridate::today(), "%Y%m%d")))
  counter_data_old <- db.download_counter()
  counter_data_old <- counter_data_old %>%
    update_til_now(now=now)

  counter_data_new <- counter_data %>%
    update_til_now(now=now)


  diff <- counter_data_new %>%
    tidyr::pivot_longer(-date, names_to="indicator") %>%
    mutate(source="new") %>%
    bind_rows(
      counter_data_old %>%
        tidyr::pivot_longer(-date, names_to="indicator") %>%
        mutate(source="old")
    ) %>%
    tidyr::pivot_wider(names_from="source") %>%
    mutate(diff=new-old)

  diff_total <- diff %>% filter(indicator=="cumulated_total_eur") %>% pull(diff)

  ratio <- max(-1, min(1, max_diff_per_update/diff_total))

  counter_data_updated <- diff %>%
    mutate(updated = ifelse(grepl("cumulated", indicator), old + diff * ratio, new)) %>%
    select(date, indicator, updated) %>%
    tidyr::pivot_wider(values_from="updated",
                       names_from="indicator")

  ok <- ok & all(counter_data_updated > 0 )
  ok <- ok & (abs((counter_data_updated$cumulated_coal_eur +
                     counter_data_updated$cumulated_oil_eur +
                     counter_data_updated$cumulated_gas_eur) -
                    counter_data_updated$cumulated_total_eur) < 1)

  if(ok){
    print("Updating counter data")
    db.update_counter(counter_data_updated)
    if(!is.null(prices)){
      db.update_counter_prices(prices)
    }
  }
}

