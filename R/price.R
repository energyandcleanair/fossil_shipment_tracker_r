prices.get_predicted_portprices <- function(production=F){

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

  eur_per_usd <- price.eur_per_usd(date_from=min(brent$date), date_to=min(max(brent$date), lubridate::today()))
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
                   portprice_espo) %>%
           mutate(scenario='default'))
}


prices.get_predicted_prices <- function(production=F){

  prices_daily <- get_prices_daily(running_days=30) %>%
    arrange(desc(date)) %>%
    filter(!is.na(date))

  if(production){
    suffix=''
  }else{
    suffix='_development'
  }

  models_eu <- readRDS(system.file("extdata", sprintf("pricing_models_eu%s.RDS", suffix), package="russiacounter"))
  models_noneu <- readRDS(system.file("extdata", sprintf("pricing_models_noneu%s.RDS", suffix), package="russiacounter"))

  prices_eu <- models_eu %>%
    mutate(new_data=list(prices_daily)) %>%
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
    mutate(new_data=list(prices_daily)) %>%
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

  # Extend and fill
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
    filter(!is.na(date)) %>%
    mutate(scenario='default')

  return(p)
}


price.get_capped_prices <- function(production=F){


  p <- prices.get_predicted_prices(production=production)

  # Cap using 2021H1 prices, weighted average, one globally
  caps <- readRDS(system.file("extdata", "comtrade_eurostat.RDS", package="russiacounter")) %>%
    filter(lubridate::floor_date(date,'halfyear')=='2021-01-01') %>%
    group_by(commodity, unit) %>%
    summarise_at('value', sum, na.rm=T) %>%
    tidyr::spread(unit, value) %>%
    mutate(eur_per_tonne=eur/tonne) %>%
    select(-c(eur, tonne)) %>%
    mutate(commodity=recode(commodity,
                             oil='crude_oil'))

  caps <- bind_rows(caps, caps %>%
                      filter(commodity=='crude_oil') %>%
                      mutate(commodity='pipeline_oil'))

  eur_per_usd <- price.eur_per_usd(date_from=min(p$date),
                                   date_to=min(max(p$date), lubridate::today()))

  eur_per_usd_2021H1 <- eur_per_usd %>%
    filter(lubridate::floor_date(date,'halfyear')=='2021-01-01') %>%
    summarise(eur_per_usd_base=mean(eur_per_usd))

  eur_per_usd %>%
    tidyr::crossing(eur_per_usd_2021H1) %>%
    mutate(price_adjustment = eur_per_usd / eur_per_usd_base) %>%
    select(date, price_adjustment) %>%
    tidyr::crossing(caps) %>%
    mutate(eur_per_tonne=case_when(commodity %in% c('natural_gas', 'lng') ~ eur_per_tonne,
                                   T ~ eur_per_tonne * price_adjustment)) %>%
    select(-c(price_adjustment)) %>%
    rename(max_eur_per_tonne=eur_per_tonne) %>%
    ungroup() %>%
    right_join(p) %>%
    mutate(eur_per_tonne=case_when(
      (date >= '2022-07-01') ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
      T ~ eur_per_tonne),
           scenario='pricecap') %>%
    select(-c(max_eur_per_tonne))







  # Oil: USD 50/barrel ; Price Cap - USD 55/barrel
  # Piped Gas: USD 7.3/MWh ; Price Cap - USD 15/MWh
  # LNG: USD 25/MWh ; Price Cap - USD 30/MWh
  # Coal: USD 40/ton ; Price Cap - USD 50/ton
  # ng_mwh_per_tonne <- 12.54 #https://unit-converter.gasunie.nl/
  # barrel_per_tonne <- 7.49
  #
  # caps <- list(
  #   crude_oil=55 * barrel_per_tonne,
  #   natural_gas= 15 * ng_mwh_per_tonne,
  #   lng= 30 * ng_mwh_per_tonne,
  #   coal=50
  # )
  #
  # eur_per_usd <- price.eur_per_usd(date_from=min(p$date),
  #                                  date_to=min(max(p$date), lubridate::today()))
  #
  #
  # tibble(commodity=names(caps),
  #        usd_per_tonne=unlist(caps)) %>%
  #   tidyr::crossing(eur_per_usd) %>%
  #   arrange(date) %>%
  #   tidyr::fill(eur_per_usd) %>%
  #   mutate(max_eur_per_tonne=usd_per_tonne*eur_per_usd) %>%
  #   select(-c(usd_per_tonne, eur_per_usd)) %>%
  #   right_join(p) %>%
  #   mutate(eur_per_tonne=case_when(
  #     (date >= '2022-07-01') ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
  #     T ~ eur_per_tonne),
  #          scenario='pricecap') %>%
  #   select(-c(max_eur_per_tonne))
}

price.get_capped_portprices <- function(production=F){


  p <- prices.get_predicted_portprices(production=production)

  # Cap using 2021H1 prices, weighted average, one globally
  caps <- readRDS(system.file("extdata", "comtrade_eurostat.RDS", package="russiacounter")) %>%
    filter(lubridate::floor_date(date,'halfyear')=='2021-01-01') %>%
    group_by(commodity, unit) %>%
    summarise_at('value', sum, na.rm=T) %>%
    tidyr::spread(unit, value) %>%
    mutate(eur_per_tonne=eur/tonne) %>%
    select(-c(eur, tonne)) %>%
    mutate(commodity=recode(commodity,
                            oil='crude_oil'))

  caps <- bind_rows(caps, caps %>%
                      filter(commodity=='crude_oil') %>%
                      mutate(commodity='pipeline_oil'))

  eur_per_usd <- price.eur_per_usd(date_from=min(p$date),
                                   date_to=min(max(p$date), lubridate::today()))

  eur_per_usd_2021H1 <- eur_per_usd %>%
    filter(lubridate::floor_date(date,'halfyear')=='2021-01-01') %>%
    summarise(eur_per_usd_base=mean(eur_per_usd))

  eur_per_usd %>%
    tidyr::crossing(eur_per_usd_2021H1) %>%
    mutate(price_adjustment = eur_per_usd / eur_per_usd_base) %>%
    select(date, price_adjustment) %>%
    tidyr::crossing(caps) %>%
    mutate(eur_per_tonne=case_when(commodity %in% c('natural_gas', 'lng') ~ eur_per_tonne,
                                   T ~ eur_per_tonne * price_adjustment)) %>%
    select(-c(price_adjustment)) %>%
    rename(max_eur_per_tonne=eur_per_tonne) %>%
    ungroup() %>%
    right_join(p) %>%
    mutate(eur_per_tonne=case_when(
      (date >= '2022-07-01') ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
      T ~ eur_per_tonne),
      scenario='pricecap') %>%
    select(-c(max_eur_per_tonne))

  # ng_mwh_per_tonne <- 12.54 #https://unit-converter.gasunie.nl/
  # barrel_per_tonne <- 7.49
  #
  # caps <- list(
  #   crude_oil=55 * barrel_per_tonne,
  #   natural_gas= 15 * ng_mwh_per_tonne,
  #   lng= 30 * ng_mwh_per_tonne,
  #   coal=50
  # )
  #
  # eur_per_usd <- price.eur_per_usd(date_from=min(p$date),
  #                                  date_to=min(max(p$date), lubridate::today()))
  #
  #
  # tibble(commodity=names(caps),
  #        usd_per_tonne=unlist(caps)) %>%
  #   tidyr::crossing(eur_per_usd) %>%
  #   arrange(date) %>%
  #   tidyr::fill(eur_per_usd) %>%
  #   mutate(max_eur_per_tonne=usd_per_tonne*eur_per_usd) %>%
  #   select(-c(usd_per_tonne, eur_per_usd)) %>%
  #   right_join(p) %>%
  #   mutate(eur_per_tonne=case_when(
  #     (date >= '2022-07-01') ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
  #     T ~ eur_per_tonne),
  #     scenario='pricecap') %>%
  #   select(-c(max_eur_per_tonne))

}


# prices.add_tail <- function(p, add_tail_days){
#   if(is.null(add_tail_days) | add_tail_days==0){
#     return(p)
#   }
#
#   p %>%
#     group_by(commodity, country_iso2) %>%
#     arrange(desc(date)) %>%
#     top_n(7, w) %>%
#     ungroup() %>%
#     arrange(desc(country_iso2))
# }

price.check_prices <- function(p){
  ok <- !any(is.na(p$eur_per_tonne))
  ok <- ok & !any(is.na(p$scenario))
  ok <- ok & all(p$eur_per_tonne >= 0)
  ok <- ok & all(c("country_iso2","date","commodity","eur_per_tonne","scenario") %in% names(p))
  ok <- ok & nrow(p>0)
  return(ok)
}

price.check_portprices <- function(p){
  ok <- !any(is.na(p$eur_per_tonne))
  ok <- ok & !any(is.na(p$scenario))
  ok <- ok & all(p$eur_per_tonne >= 0)
  ok <- ok & all(c("port_id","date","commodity","eur_per_tonne","scenario") %in% names(p))
  ok <- ok & nrow(p>0)
  return(ok)
}

prices.update_constant_prices <- function(production=F){

  current_p <- prices.get_predicted_prices(production=production)

  date_from <- '2022-01-01'
  date_to <- '2022-02-24'
  dates <- unique(current_p$date)

  constant_p <- current_p %>%
    filter(date >= date_from,
           date <= date_to) %>%
    group_by(country_iso2, commodity) %>%
    summarise_at('eur_per_tonne', mean, na.rm=T) %>%
    tidyr::crossing(dates) %>%
    mutate(type='constant')

  ok <- price.check_prices(p)

  if(ok){
    db.upload_prices_to_posgres(p, production=production)
  }else{
    print("ERROR: prices not updated")
  }
}

price.update_prices <- function(production=F){
  p <- prices.get_predicted_prices(production=production)
  ok <- price.check_prices(p)
  if(ok){
    db.upload_prices_to_posgres(p, production=production)
  }else{
    print("ERROR: prices not updated")
  }

  capped_p <- price.get_capped_prices(production=production)
  ok <- price.check_prices(capped_p)
  if(ok){
    db.upload_prices_to_posgres(capped_p, production=production)
  }else{
    print("ERROR: capped prices not updated")
  }
}

price.update_portprices <- function(production=F){
  p <- prices.get_predicted_portprices(production=production)
  ok <- price.check_portprices(p)
  if(ok){
    db.upload_portprices_to_posgres(p, production=production)
  }else{
    print("ERROR: prices not updated")
  }

  capped_p <- price.get_capped_portprices(production=production)
  ok <- price.check_portprices(capped_p)
  if(ok){
    db.upload_portprices_to_posgres(capped_p, production=production)
  }else{
    print("ERROR: prices not updated")
  }
}



