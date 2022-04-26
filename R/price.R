prices.get_predicted_portprices <- function(){

  # Ports
  ports <- read_csv("https://api.russiafossiltracker.com/v0/port?format=csv&iso2=RU")
  # ports <- read_csv("http://127.0.0.1:8080/v0/port?format=csv&iso2=RU")

  ports_ural <- ports %>%
    filter(iso2=="RU", check_departure) %>%
    filter(lon < 40)

  ports_espo <- ports %>%
    filter(iso2=="RU", check_departure) %>%
    filter(lon > 40)

  # Discounts
  brent <- get_brent()

  eur_per_usd <- price.eur_per_usd(date_from=min(brent$date), date_to=max(brent$date))
  tonne_per_bbl <- 0.138

  spread_ural <- get_ural_brent_spread() %>%
    select(date, add_brent=usd_per_bbl)

  price_ural <- brent %>%
    left_join(spread_ural) %>%
    arrange(desc(date)) %>%
    mutate(usd_per_bbl = brent + add_brent) %>%
    left_join(eur_per_usd) %>%
    mutate(eur_per_tonne=usd_per_bbl * eur_per_usd / tonne_per_bbl) %>%
    select(date, eur_per_tonne) %>%
    mutate(commodity="crude_oil")


  spread_espo <- get_espo_brent_spread() %>%
    select(date, add_brent=usd_per_bbl)

  price_espo <- brent %>%
    left_join(spread_espo) %>%
    arrange(desc(date)) %>%
    mutate(usd_per_bbl = brent + add_brent) %>%
    left_join(eur_per_usd) %>%
    mutate(eur_per_tonne=usd_per_bbl * eur_per_usd / tonne_per_bbl) %>%
    select(date, eur_per_tonne) %>%
    mutate(commodity="crude_oil")

  portprice_ural <- ports_ural %>%
    select(port_id=id) %>%
    crossing(price_ural) %>%
    filter(!is.na(eur_per_tonne))


  portprice_espo <- ports_espo %>%
    select(port_id=id) %>%
    crossing(price_espo) %>%
    filter(!is.na(eur_per_tonne))

  return(bind_rows(portprice_ural,
                   portprice_espo))
}


prices.get_predicted_prices <- function(add_to_predictors=NULL){

  prices_monthly <- get_prices_monthly() %>%
    arrange(desc(date)) %>%
    filter(!is.na(date))

  if(!is.null(add_to_predictors)){
    #TODO make it automatic for all predictors. Only for brent for now
    prices_monthly <- prices_monthly %>%
      left_join(add_to_predictors %>%
                  group_by(date=lubridate::floor_date(date, "month")) %>%
                  summarise(brent=mean(brent, na.rm=T)) %>%
                  rename(add_to_brent=brent)) %>%
      mutate(brent=brent + tidyr::replace_na(add_to_brent,0))
  }

  models_eu <- readRDS(system.file("extdata", "pricing_models_eu.RDS", package="russiacounter"))
  models_noneu <- readRDS(system.file("extdata", "pricing_models_noneu.RDS", package="russiacounter"))

  prices_eu <- models_eu %>%
    mutate(new_data=list(prices_monthly)) %>%
    group_by(commodity) %>%
    group_map(function(df, group) {
      model <- df$model[[1]]
      new_data <- df$new_data[[1]] %>% arrange(date)
      if(is.null(model)){ return(NULL) }
      new_data$price_eur_per_tonne <- predict(model, new_data)
      new_data$price_eur_per_tonne <- pmin(new_data$price_eur_per_tonne, df$price_ceiling)
      new_data$price_ceiling <- df$price_ceiling
      tibble(group, new_data)
    }) %>% do.call(bind_rows, .) %>%
    arrange(desc(date))

  prices_eu <- prices_eu %>%
    tidyr::crossing(tibble(country_iso=codelist$iso2c[which(codelist$eu28=="EU")] ))

  prices_noneu  <- models_noneu %>%
    mutate(new_data=list(prices_monthly)) %>%
    group_by(commodity, country, country_iso) %>%
    group_map(function(df, group) {
      model <- df$model[[1]]
      new_data <- df$new_data[[1]] %>% arrange(date)
      if(is.null(model)){ return(NULL) }
      new_data$price_eur_per_tonne <- predict(model, new_data)
      new_data$price_eur_per_tonne <- pmin(new_data$price_eur_per_tonne, df$price_ceiling)
      new_data$price_ceiling <- df$price_ceiling
      tibble(group, new_data)
    }) %>% do.call(bind_rows, .)

  p <- bind_rows(prices_eu,
                 prices_noneu) %>%
    dplyr::select(country_iso2=country_iso,
                  date,
                  commodity,
                  eur_per_tonne=price_eur_per_tonne) %>%
    mutate(date=as.Date(date))

  # Add pipeline_oil equivalent to crude_oil
  p <- bind_rows(p,
                 p %>% filter(commodity=="crude_oil") %>% mutate(commodity="pipeline_oil"))

  # Monthly -> daily
  days_buffer <- 8
  dates <- seq.Date(min(p$date, na.rm=T), lubridate::today() + days_buffer, by="day")
  filler <- tidyr::crossing(tibble(date=dates),
                            p %>% distinct(commodity, country_iso2))
  p <- p %>%
    full_join(filler, by=c("commodity","country_iso2","date")) %>%
    group_by(country_iso2, commodity) %>%
    arrange(date) %>%
    fill(eur_per_tonne) %>%
    filter(!is.na(eur_per_tonne)) %>%
    filter(!is.na(date))

  return(p)
}


price.check_prices <- function(p){
  ok <- !any(is.na(p$eur_per_tonne))
  ok <- ok & all(p$eur_per_tonne >= 0)
  ok <- ok & all(c("country_iso2","date","commodity","eur_per_tonne") %in% names(p))
  ok <- ok & nrow(p>0)
  return(ok)
}


price.update_prices <- function(){
  p <- prices.get_predicted_prices()
  ok <- price.check_prices(p)
  if(ok){
    db.upload_prices_to_posgres(p)
  }
}

price.update_portprices <- function(){
  p <- prices.get_predicted_portprices()
  ok <- price.check_prices(p)
  if(ok){
    db.upload_portprices_to_posgres(p)
  }
}

price.get_modelled_price <- function(flows_entsog, flows_comtrade_eurostat, cap_price=T){

  flows <- bind_rows(
    flows_entsog %>%
      filter(unit=="MWh/day") %>%
      mutate(value=value/gcv_MWh_per_m3*kg_per_m3/1000,
             unit="tonne",
             transport="pipeline") %>%
      group_by(date, commodity, transport, unit, source) %>%
      summarise(value=sum(value)) %>%
      mutate(country="All"),

    # Spread on a daily basis as opposed to a monthly one
    flows_comtrade_eurostat %>%
      filter(country=="EU") %>%
      filter(unit %in% c("tonne","eur")) %>%
      tidyr::pivot_wider(names_from="unit", names_prefix="value_", values_from="value") %>%
      mutate(price=value_eur/value_tonne) %>%
      rename(value=value_tonne) %>%
      mutate(unit="tonne") %>%
      select(-c(value_eur)) %>%
      filter(paste(commodity) != paste("natural_gas")) %>%
      right_join(
        tibble(date_day=seq(min(flows_comtrade_eurostat$date),
                            lubridate::ceiling_date(max(flows_comtrade_eurostat$date), "month") - 1, by="day")) %>%
          mutate(date=lubridate::floor_date(date_day, 'month'))
      ) %>%
      mutate(value=value / lubridate::days_in_month(date)) %>%
      mutate(date=date_day) %>%
      select(-c(date_day))
  ) %>%
    mutate(month=lubridate::floor_date(date, "month"))

  # Get pricing
  flows_month <- flows %>%
    distinct(date=month, transport, commodity, price)

  # Preparing a capping price
  price_cap <- flows_comtrade_eurostat %>%
    filter(country=="EU") %>%
    filter(date>="2019-01-01") %>% # There are weird prices before that, around 15,000 eur/tonne
    filter(commodity %in% c('natural_gas','lng')) %>%
    select(country, date, commodity, unit, value) %>%
    tidyr::spread(unit, value) %>%
    mutate(price=eur/tonne) %>%
    group_by(commodity) %>%
    summarise(price_cap=max(price, na.rm=T))


  # models <- readRDS('data/pricing_models.RDS')
  models <- readRDS(system.file("extdata", "pricing_models.RDS", package="russiacounter"))

  # Get predictors
  ttf <- quantmod::getSymbols("TTF=F", from = '2016-01-01', warnings = FALSE, auto.assign = F)
  ttf_monthly <- ttf %>% as_tibble %>% mutate(date = ttf %>% as.data.frame %>% row.names %>% ymd) %>%
    rename(TTF = contains('Close')) %>% group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(TTF, mean, na.rm=T))

  brent <- tidyquant::tq_get("BZ=F", from='2016-01-01')
  brent_monthly <- brent %>% rename(Brent = close) %>% group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(Brent, mean, na.rm=T))

  #data downloaded from https://www.investing.com/commodities/rotterdam-coal-futures-streaming-chart
  # ara <- read_csv('data/Rotterdam_Coal_Futures_Historical_Data.csv')

  ara <- get_ara()
  ara_monthly <- ara %>% rename(ARA = ara) %>%
    group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(ARA, mean, na.rm=T))

  prices <- flows_month %>%
    filter(date >= '2016-01-01') %>%
    left_join(ttf_monthly) %>%
    left_join(brent_monthly) %>%
    left_join(ara_monthly) %>%
    group_by(commodity) %>%
    tidyr::nest() %>%
    left_join(models %>% select(-c(data)), by=c("commodity")) %>%
    group_by(commodity) %>%
    group_map(function(df, group) {

      start_year <- 2015
      model <- df$model[[1]]
      data <- df$data[[1]] %>% arrange(date)
      if(is.null(model)){ return(NULL) }
      data$price <- predict(model, data)
      tibble(group, data)
      }) %>% do.call(bind_rows, .)


  # Combine
  flows %>%
    select(-c(price)) %>%
    left_join(prices %>% rename(month=date)) %>%
    left_join(price_cap) %>%
    mutate(price_cap=tidyr::replace_na(price_cap, +Inf)) %>%
    mutate(value_eur=value * ifelse(cap_price, pmin(price, price_cap), price))
}



