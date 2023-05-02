price.get_prices <- function(production = F){

  # This function gets all prices in one dataframe,
  # those 'default' ones, the ones specific to port,
  # and the capped one by ship_onwer / insurer / destination

  # Get default values
  prices_daily_30day <- get_prices_daily(running_days = 30)
  p_default <- price.get_capped_prices(production = production, scenario='default', version='default', prices_daily_30day=prices_daily_30day)
  # p_2021H1 <- price.get_capped_prices(production = production, scenario='2021H1', version='2021H1', prices_daily_30day=prices_daily_30day)
  # p_usd20 <- price.get_capped_prices(production = production, scenario='usd20', version='usd20', prices_daily_30day=prices_daily_30day)
  # p_usd30 <- price.get_capped_prices(production = production, scenario='usd30', version='usd30', prices_daily_30day=prices_daily_30day)
  # p_usd35 <- price.get_capped_prices(production = production, scenario='usd35', version='usd35', prices_daily_30day=prices_daily_30day)
  # p_usd40 <- price.get_capped_prices(production = production, scenario='usd40', version='usd40', prices_daily_30day=prices_daily_30day)

  # Eurostat pricing for KPLER
  p_default_eurostat <- price_models_eurostat.get_predicted_prices(production=production)


  # Fill old values with NULL, because some endpoints expect a pricing_scenario
  p_default <- p_default %>%
    tidyr::complete(date=seq(as.Date('2015-01-01'), max(p_default$date), by='day'),
                    scenario,
                    commodity,
                    fill=list(eur_per_tonne=NA))

  all_prices <- bind_rows(p_default,
                          p_default_eurostat,
                          # p_2021H1,
                          # p_usd20,
                          # p_usd30,
                          # p_usd40
                          )

  return(all_prices)
}

price.get_scenario_names <- function(){
  tibble(id=c('default', '2021H1', 'usd20', 'usd30', 'usd40'),
         name=c('USD60/bbl (actual)', '2021H1 prices', 'USD20/bbl', 'USD30/bbl', 'USD40/bbl'))
}

price.get_predicted_portprices <- function(production=F){

  # Ports
  ports <- read_csv("https://api.russiafossiltracker.com/v0/port?format=csv&iso2=RU", show_col_type=F)

  ports_ural <- ports %>%
    filter(iso2=="RU", check_departure) %>%
    filter(lon < 40)

  ports_espo <- ports %>%
    filter(iso2=="RU", check_departure) %>%
    filter(lon > 40)

  # Discounts
  brent <- russiacounter::get_brent()

  eur_per_usd <- price.eur_per_usd(date_from=min(brent$date), date_to=min(max(brent$date), lubridate::today()))
  tonne_per_bbl <- 0.138

  spread_ural <- russiacounter::get_ural_brent_spread() %>%
    select(date, add_brent=usd_per_bbl) %>%
    tidyr::complete(date=seq.Date(min(date), max(date), by='day')) %>%
    fill(add_brent)

  price_ural <- brent %>%
    left_join(spread_ural) %>%
    arrange(desc(date)) %>%
    mutate(usd_per_bbl = brent + add_brent) %>%
    left_join(eur_per_usd) %>%
    mutate(eur_per_tonne=usd_per_bbl * eur_per_usd / tonne_per_bbl) %>%
    select(date, eur_per_tonne) %>%
    mutate(commodity="crude_oil")

  spread_espo <- get_espo_brent_spread() %>%
    select(date, add_brent=usd_per_bbl) %>%
    arrange(date) %>%
    tidyr::complete(date=seq.Date(min(date), max(date), by='day')) %>%
    fill(add_brent)

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

  pp <- bind_rows(portprice_ural,
                   portprice_espo) %>%
           mutate(scenario='default')

  # Nest country to match new price table structure
  pp_formatted <- pp %>%
    group_by(date, commodity, eur_per_tonne, scenario) %>%
    summarise(departure_port_ids=list(port_id)) %>%
    ungroup()

  return(pp_formatted)
}


price.get_predicted_prices <- function(production=F, prices_daily_30day=NULL){

  if(is.null(prices_daily_30day)){
    prices_daily_30day <- get_prices_daily(running_days=30)
  }

  prices_daily_30day <- prices_daily_30day %>%
    arrange(desc(date)) %>%
    filter(!is.na(date)) %>%
    mutate(date=as.Date(date))

  if(production){
    suffix=''
  }else{
    suffix='_development'
  }

  models_eu <- readRDS(system.file("extdata", sprintf("pricing_models_eu%s.RDS", suffix), package="russiacounter"))
  models_noneu <- readRDS(system.file("extdata", sprintf("pricing_models_noneu%s.RDS", suffix), package="russiacounter"))

  prices_eu <- models_eu %>%
    mutate(new_data=list(prices_daily_30day)) %>%
    group_by(commodity) %>%
    group_map(function(df, group) {
      model <- df$model[[1]]
      new_data <- df$new_data[[1]] %>% arrange(date)
      if(is.null(model)){ return(NULL) }
      new_data$price_eur_per_tonne <- predict(model, new_data)
      new_data$price_eur_per_tonne <- pmax(pmin(new_data$price_eur_per_tonne, df$price_ceiling), df$price_floor)
      new_data$price_ceiling <- df$price_ceiling
      tibble(group, new_data)
    }) %>% do.call(bind_rows, .) %>%
    arrange(desc(date))

  prices_eu <- prices_eu %>%
    tidyr::crossing(tibble(iso2=codelist$iso2c[which(codelist$eu28=="EU")] ))

  prices_noneu  <- models_noneu %>%
    mutate(new_data=list(prices_daily_30day)) %>%
    group_by(commodity, country, iso2) %>%
    group_map(function(df, group) {
      model <- df$model[[1]]
      new_data <- df$new_data[[1]] %>% arrange(date)
      if(is.null(model)){ return(NULL) }
      new_data$price_eur_per_tonne <- predict(model, new_data)
      new_data$price_eur_per_tonne <- pmax(pmin(new_data$price_eur_per_tonne, df$price_ceiling), df$price_floor)
      new_data$price_ceiling <- df$price_ceiling
      tibble(group, new_data)
    }) %>% do.call(bind_rows, .)

  p <- bind_rows(prices_eu,
                 prices_noneu) %>%
    dplyr::select(country_iso2=iso2,
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
    tidyr::fill(eur_per_tonne) %>%
    filter(!is.na(eur_per_tonne)) %>%
    filter(!is.na(date)) %>%
    mutate(scenario='default')


  # Nest country to match new price table structure
  p_formatted <- p %>%
    filter(!is.na(country_iso2)) %>%
    group_by(date, commodity, eur_per_tonne, scenario) %>%
    summarise(destination_iso2s=list(country_iso2)) %>%
    bind_rows(p %>%
                filter(is.na(country_iso2)) %>%
                ungroup()) %>%
    ungroup() %>%
    select(-c(country_iso2))

  return(p_formatted)
}


price.get_price_caps <- function(p, version){

  if(version=='2021H1'){

    # Cap using 2021H1 prices, weighted average, one globally
    precaps <- readRDS(system.file("extdata", "comtrade_eurostat.RDS", package="russiacounter")) %>%
      filter(lubridate::floor_date(date,'halfyear')=='2021-01-01') %>%
      group_by(commodity, unit) %>%
      summarise_at('value', sum, na.rm=T) %>%
      tidyr::spread(unit, value) %>%
      mutate(eur_per_tonne=eur/tonne) %>%
      select(-c(eur, tonne)) %>%
      mutate(commodity=recode(commodity,
                              oil='crude_oil'))

    precaps <- bind_rows(precaps, precaps %>%
                        filter(commodity=='crude_oil') %>%
                        mutate(commodity='pipeline_oil'))

    eur_per_usd <- price.eur_per_usd(date_from=min(p$date),
                                     date_to=min(max(p$date), lubridate::today()))

    eur_per_usd_2021H1 <- eur_per_usd %>%
      filter(lubridate::floor_date(date,'halfyear')=='2021-01-01') %>%
      summarise(eur_per_usd_base=mean(eur_per_usd))

    caps <- eur_per_usd %>%
      tidyr::crossing(eur_per_usd_2021H1) %>%
      mutate(price_adjustment = eur_per_usd / eur_per_usd_base) %>%
      select(date, price_adjustment) %>%
      tidyr::crossing(precaps) %>%
      mutate(eur_per_tonne=case_when(commodity %in% c('natural_gas', 'lng') ~ eur_per_tonne,
                                     T ~ eur_per_tonne * price_adjustment)) %>%
      select(-c(price_adjustment)) %>%
      rename(max_eur_per_tonne=eur_per_tonne)
  }

  if(version=='andrei'){
    ng_mwh_per_tonne <- 12.54 #https://unit-converter.gasunie.nl/
    barrel_per_tonne <- 7.49

    precaps <- list(
      crude_oil=55 * barrel_per_tonne,
      natural_gas= 15 * ng_mwh_per_tonne,
      lng= 30 * ng_mwh_per_tonne,
      coal=50
    )

    eur_per_usd <- price.eur_per_usd(date_from=min(p$date),
                                     date_to=min(max(p$date), lubridate::today()))

    caps <- tibble(commodity=names(precaps),
                 usd_per_tonne=unlist(precaps)) %>%
      tidyr::crossing(eur_per_usd) %>%
      arrange(date) %>%
      tidyr::fill(eur_per_usd) %>%
      mutate(max_eur_per_tonne=usd_per_tonne*eur_per_usd) %>%
      select(-c(usd_per_tonne, eur_per_usd))
  }

  if(version=='default'){
    # ng_mwh_per_tonne <- 12.54 #https://unit-converter.gasunie.nl/
    barrel_per_tonne <- 1 / 0.138

    precaps <- list(
      crude_oil= 60 * barrel_per_tonne
    )

    eur_per_usd <- price.eur_per_usd(date_from=min(p$date),
                                     date_to=min(max(p$date), lubridate::today()))

    caps <- tibble(commodity=names(precaps),
                 usd_per_tonne=unlist(precaps)) %>%
      tidyr::crossing(eur_per_usd) %>%
      arrange(date) %>%
      tidyr::fill(eur_per_usd) %>%
      mutate(max_eur_per_tonne=usd_per_tonne*eur_per_usd) %>%
      select(-c(usd_per_tonne, eur_per_usd))
  }

  if(grepl('usd[0-9]+$', version)){
    barrel_per_tonne <- 1 / 0.138
    cap_usd <- as.numeric(sub('usd','',version))
    precaps <- list(
      crude_oil= cap_usd * barrel_per_tonne
    )

    eur_per_usd <- price.eur_per_usd(date_from=min(p$date),
                                     date_to=min(max(p$date), lubridate::today()))

    caps <- tibble(commodity=names(precaps),
                   usd_per_tonne=unlist(precaps)) %>%
      tidyr::crossing(eur_per_usd) %>%
      arrange(date) %>%
      tidyr::fill(eur_per_usd) %>%
      mutate(max_eur_per_tonne=usd_per_tonne*eur_per_usd) %>%
      select(-c(usd_per_tonne, eur_per_usd))
  }

  return(ungroup(caps))
}

price.get_capped_prices <- function(production=F, scenario='default', version='default', prices_daily_30day=NULL){

  p <- price.get_predicted_prices(production=production, prices_daily_30day=prices_daily_30day)
  pp <- price.get_predicted_portprices(production = production)
  caps <- price.get_price_caps(p=p, version=version)

  commodities <- read_csv('https://api.russiafossiltracker.com/v0/commodity?format=csv')
  seaborne_commodities <- commodities %>% filter(transport == 'seaborne') %>% pull(id)
  eu <- setdiff(codelist$iso2c[!is.na(codelist$eu28)], 'GB')
  g7 <- c('CA','FR','DE','IT','JP','GB','US','UK')
  eu_g7 <- c(eu, g7)
  destination_iso2 <- eu_g7
  ship_owner_iso2s <- eu_g7
  ship_insurer_iso2s <- eu_g7
  date_start <- c('2022-12-06')

  # Create one version per destination_iso2, with no ship constraint
  pc_destination <- caps %>%
      right_join(p) %>%
    tidyr::unnest(destination_iso2s, keep_empty = T) %>%
    mutate(eur_per_tonne=case_when(
        (date >= date_start) & (destination_iso2s %in% destination_iso2) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
        T ~ eur_per_tonne),
        # Not to have NULL nested with other countries
        destination_is_null=is.null(destination_iso2s) | is.na(destination_iso2s)) %>%
    # To nest more together
    mutate(eur_per_tonne=round(eur_per_tonne, 3)) %>%
    group_by(across(-c(destination_iso2s, max_eur_per_tonne))) %>%
    summarise(across(destination_iso2s, list)) %>%
    select(-c(destination_is_null)) %>%
    ungroup()

  # Create a ship constraint (owner): regardless of destination and insurer
  pc_ship_owner <- pc_destination %>%
    filter(commodity %in% seaborne_commodities) %>%
    left_join(caps) %>%
    mutate(eur_per_tonne=case_when(
           (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
           T ~ eur_per_tonne),
           ship_owner_iso2s=list(ship_owner_iso2s))

  # Create a ship constraint (insurer): regardless of destination and owner
  pc_ship_insurer <- pc_destination %>%
    filter(commodity %in% seaborne_commodities) %>%
    left_join(caps) %>%
    mutate(eur_per_tonne=case_when(
      (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
      T ~ eur_per_tonne),
      ship_insurer_iso2s=list(ship_insurer_iso2s))

  # Port prices for destinations
  ppc_destination <- caps %>%
    filter(commodity %in% seaborne_commodities) %>%
    right_join(pp) %>%
    mutate(eur_per_tonne=case_when(
      (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
      T ~ eur_per_tonne),
      destination_iso2s=list(destination_iso2))

  # Port prices for ship_owner
  ppc_ship_owner <- caps %>%
    filter(commodity %in% seaborne_commodities) %>%
    right_join(pp) %>%
    mutate(eur_per_tonne=case_when(
      (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
      T ~ eur_per_tonne),
      ship_owner_iso2s=list(ship_owner_iso2s))

  # Port prices for ship_insurer
  ppc_ship_insurer <- caps %>%
    filter(commodity %in% seaborne_commodities) %>%
    right_join(pp) %>%
    mutate(eur_per_tonne=case_when(
      (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm=T),
      T ~ eur_per_tonne),
      ship_insurer_iso2s=list(ship_insurer_iso2s))


  pc <- bind_rows(ppc_ship_owner,
                  ppc_ship_insurer,
                  ppc_destination,
                  pc_destination,
                  pc_ship_owner,
                  pc_ship_insurer,
                  pp
                  ) %>%
    ungroup() %>%
    select(-c(max_eur_per_tonne)) %>%
    mutate(scenario=!!scenario)

  return(pc)
}


price.get_capped_portprices <- function(production=F){

  p <- price.get_predicted_portprices(production=production)

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
}


price.check_prices <- function(p){
  ok <- !any(is.na(p$eur_per_tonne) & (p$date >= '2018-01-01'))
  ok <- ok & !any(is.na(p$scenario))
  ok <- ok & (min(p$eur_per_tonne, na.rm=T) >= 0)
  ok <- ok & all(c("destination_iso2s", "departure_port_ids", "ship_owner_iso2s", "ship_insurer_iso2s", "date","commodity","eur_per_tonne","scenario") %in% names(p))
  ok <- ok & nrow(p) >0
  return(ok)
}


price.update_prices <- function(production=F, buffer_days=60, rebuild=F){
  prices <- price.get_prices(production=production)
  ok <- price.check_prices(prices)
  if(ok){
    db.upload_prices_to_posgres(prices, production=production, buffer_days=buffer_days, rebuild=rebuild)
  }else{
    print("ERROR: prices not updated")
  }

  scenario_names <- price.get_scenario_names()
  db.upload_scenario_names(scenario_names, production=production)
}



