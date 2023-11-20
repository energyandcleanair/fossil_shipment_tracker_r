price_models_comtrade.build <- function(production=F, refresh_comtrade=T, diagnostic_folder='diagnostics'){

  #TODO Clean that up
  library(tidyverse)
  library(magrittr)
  library(lubridate)
  library(countrycode)


  # Collect predictors ------------------------------------------------------
  prices_monthly <- get_prices_monthly()

  # Collect trade data ----------------------------------------------------------
  trade <- price_models_comtrade.get_trade(prices_monthly=prices_monthly, refresh_comtrade = refresh_comtrade)

  # Europe ------------------------------------------------------------------
  # We take EU reporting to comtrade, as a whole, not the sume of EU countries
  trade_eu <- trade %>%
    filter(country=="EU") %>%
    group_by(commodity, date) %>%
    summarise(across(c(value_eur, value_kg), sum, na.rm=T),
              across(all_of(c('world_price_eur_per_tonne', prices_monthly %>% select(where(is.numeric)) %>% names)), unique)) %>%
    mutate(price_eur_per_tonne = value_eur/(value_kg/1000)) %>%
    mutate(date=as.Date(date))

  trade_with_predictions_eu <- trade_eu %>%
    group_by(commodity) %>%
    group_map(function(df, group) {
      print(group$commodity)
      start_year = ifelse(group$commodity=='natural_gas', 2016, ifelse(group$commodity=='lng', 2018, 2015))
      independents = case_when(group$commodity=='coal' ~ 'ara',
                               group$commodity=='natural_gas' ~
                                 'brent + ttf + lag(ttf)',
                               group$commodity=='lng' ~ 'ttf + lag(ttf)',
                               grepl('oil', group$commodity) ~
                                 'brent + lag(brent) + lag(brent, 3)',
                               group$commodity=='lpg' ~ 'brent + ttf + lag(ttf)',
                               grepl('oil', group$commodity)  ~
                                 'brent + lag(brent) + lag(brent, 3) + 0',
                               T ~ 'brent + lag(price_eur_per_tonne, 12)')

      df <- df %>% arrange(date) %>% filter(year(date)>=start_year)
      m <- df %>%
        lm(as.formula(paste('price_eur_per_tonne ~', independents)), data=.)

      tibble_row(data=list(as.data.frame(df %>% mutate(predicted_price = predict(m, df)))),
                 model=list(m),
                 commodity=group$commodity,
                 price_ceiling = max(df$price_eur_per_tonne),
                 price_floor = min(df$price_eur_per_tonne),)
    }) %>% do.call(bind_rows, .)

  if(!is.null(diagnostic_folder)){
    plt <- trade_with_predictions_eu %>%
      select(-c(model)) %>%
      tidyr::unnest(data)  %>% pivot_longer(c(price_eur_per_tonne, predicted_price, price_ceiling)) %>% filter(year(date)>=2016) %>%
      ggplot(aes(date, value, col=name)) +
      facet_wrap(~commodity) + geom_line()

    ggsave(file.path(diagnostic_folder, 'pricing_model_eu.png'), plot=plt, width=12, height=8)

    plt <- trade_with_predictions_eu %>% select(-c(model)) %>%
      tidyr::unnest(data)  %>% filter(year(date)>=2016) %>%
      ggplot(aes(price_eur_per_tonne, predicted_price)) +
      facet_wrap(~commodity, scales='free') + geom_point() + geom_abline() + geom_smooth()
    ggsave(file.path(diagnostic_folder, 'pricing_model_eu_diagnostics.png'), plot=plt, width=12, height=8)
  }

  suffix <- ifelse(production, '', '_development')
  saveRDS(trade_with_predictions_eu, sprintf('inst/extdata/pricing_models_eu%s.RDS',suffix))
  saveRDS(trade_with_predictions_eu, sprintf('data/pricing_models_eu%s.RDS',suffix))

  # Non-Europe --------------------------------------------------------------
  small_transit_countries <- c('Armenia', 'Belarus')
  top_importers <- trade %>%
    filter(!EU, !is.na(iso2), year(date)>=2017) %>%
    filter(!country %in% small_transit_countries) %>%
    group_by(country, commodity) %>%
    summarise(value_eur=mean(value_eur, na.rm=T),
              n=n(),
              last_date=max(date)) %>%
    group_by(commodity) %>%
    mutate(share = value_eur/sum(value_eur)) %>%
    filter(n>10, share>.025, last_date >= '2021-01-01')

  trade_grouped <- top_importers %>% select(-value_eur) %>% left_join(trade)
  trade_grouped <- trade %>%
    filter(!is.na(iso2), country!='EU') %>%
    group_by(commodity, date, country = ifelse(EU, 'EU', 'others')) %>%
    summarise(across(c(value_eur, value_kg), sum, na.rm=T),
              across(all_of(c('world_price_eur_per_tonne', prices_monthly %>% select(is.numeric) %>% names)), unique)) %>%
    mutate(price_eur_per_tonne = value_eur/(value_kg/1000)) %>%
    bind_rows(trade_grouped) %>%
    mutate(date=as.Date(date))

  trade_with_predictions <- trade_grouped %>%
    group_by(commodity, country, iso2) %>%
    group_map(function(df, group) {
      message(group)
      start_year = ifelse(group$commodity %in% c('lng', 'coal'), 2016, 2015) #Russia lng & coal prices in 2015 were silly
      message(start_year)
      max_deviation = 10
      independents = case_when(group$commodity=='coal' ~ 'ara + global_coal',
                               group$commodity=='natural_gas' ~
                                 'brent + ttf + jkm',
                               group$commodity=='lng' ~ 'ttf + jkm',
                               group$commodity=='lpg' ~ 'brent + ttf + jkm',
                               grepl('oil', group$commodity)  ~
                                 'brent + lag(brent) + lag(brent, 3) + 0',
                               T ~ 'brent')

      df <- df %>% filter(price_eur_per_tonne/world_price_eur_per_tonne < max_deviation) %>%
        filter(year(date)>=start_year)
      df %>% arrange(date) %>%
        lm(as.formula(paste('price_eur_per_tonne ~', independents)), data=.) -> m

      tibble_row(data=list(as.data.frame(df %>% mutate(predicted_price = predict(m, df)))),
                 model=list(m),
                 group,
                 price_ceiling = max(df$world_price_eur_per_tonne),
                 price_floor = min(df$world_price_eur_per_tonne),)
    }) %>% do.call(bind_rows, .)

  trade_with_predictions %>% select(-c(model)) %>%
    tidyr::unnest(data) ->
    trade_w_pred_df

  if(trade_w_pred_df %>%
     filter(predicted_price < 0) %>%
     nrow() >0){
    stop("Some predicted prices are negative")
  }

  if(!is.null(diagnostic_folder)){
    plt <- trade_w_pred_df %>% pivot_longer(matches('price')) %>%
      filter(year(date)>=2016) %>%
      filter(name %in% c("price_eur_per_tonne", "predicted_price")) %>%
      ggplot(aes(date, value, col=name)) + facet_grid(commodity~country, scales='free_y') + geom_line()

    ggsave(file.path(diagnostic_folder, 'pricing_model_row.png'), plot=plt, width=16, height=12)

    plt <- trade_w_pred_df %>% filter(year(date)>=2016) %>%
      ggplot(aes(price_eur_per_tonne, predicted_price, col=country)) +
      facet_wrap(~commodity, scales='free') + geom_point() + geom_abline()+ geom_smooth()
    ggsave(file.path(diagnostic_folder, 'pricing_model_row_diagnostics.png'), plot=plt)
  }

  trade_with_predictions_non_eu <- trade_with_predictions %>%
    filter(country != 'EU')

  saveRDS(trade_with_predictions_non_eu,
          sprintf('inst/extdata/pricing_models_noneu%s.RDS', suffix))
  saveRDS(trade_with_predictions_non_eu,
          sprintf('data/pricing_models_noneu%s.RDS',suffix))

  trade_with_predictions %>% rowwise() %>% group_split() %>%
    lapply(function(df) {
      tibble(df, date=prices_monthly$date, predicted_price = predict(df$model[[1]], prices_monthly))
    }) %>% bind_rows() %>% filter(year(date)==2022, month(date)>=2) %>%
    group_by(country, commodity, date) %>% summarise(across(predicted_price, mean)) %>% na.omit() %>%
    write_csv('data/predicted_export_prices.csv')
}

price_models_comtrade.get_predicted <- function(production=F, prices_daily_30day=NULL){

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
    filter(!is.na(date))


  # Nest country to match new price table structure
  p_formatted <- p %>%
    filter(!is.na(country_iso2)) %>%
    group_by(date, commodity, eur_per_tonne) %>%
    summarise(destination_iso2s=list(country_iso2)) %>%
    bind_rows(p %>%
                filter(is.na(country_iso2)) %>%
                ungroup()) %>%
    ungroup() %>%
    select(-c(country_iso2))

  return(p_formatted)
}

price_models_comtrade.get_trade <- function(prices_monthly, refresh_comtrade=T){

  oil_codes <- c("2709","2710")
  gas_codes <- c("2711","271121","271111", "271119")
  coal_codes <- c("2701","2704","2705","2706")

  # Adding more granular data for kpler
  # fueloils_codes <- c("27101951", "27101962", "27101966", "27101967")
  # kerosene_codes <- c("27101921", "27101925")
  # diesel_codes <- c("27101931", "27101935", "27101943", "27101946", "27101947",
  #                   "27101948", "27102011", "27102016", "27102019")
  # gasoline_codes <- c("271012")

  if(refresh_comtrade){
    imp <- utils.collect_comtrade(partners="Russian Federation",
                                  reporters=c("all", "EUR"),
                                  trade_flow="import",
                                  years=seq(2016, lubridate::year(lubridate::today())),
                                  codes=c(coal_codes, oil_codes, gas_codes),
                                  frequency="monthly",
                                  stop_if_no_row=F)
    saveRDS(imp, "cache/imp_for_building_models.RDS")

    exp <- utils.collect_comtrade(partners="all",
                                  reporters="Russian Federation",
                                  trade_flow="export",
                                  years=seq(2015,2019), # No record for 2020, and 2021
                                  codes=c(coal_codes, oil_codes, gas_codes),
                                  frequency="monthly",
                                  stop_if_no_row=F)
    saveRDS(exp, "cache/exp_for_building_models.RDS")
  }else{
    imp <-readRDS( "cache/imp_for_building_models.RDS")
    exp <- readRDS("cache/exp_for_building_models.RDS")
  }

  clean_comtrade <- function(df, is_import){
    df <- df %>%
      rename(value_kg=net_wgt,
             value_usd=primary_value,
             commodity=cmd_desc,
             reporter_iso3=reporter_iso,
             partner_iso3=partner_iso) %>%
      filter(!is.na(period)) %>%
      mutate(across(is.logical, as.character),
             across(matches('_kg|_usd|year|period|flag'), as.numeric),
             across(matches('code|level'), as.character)) %>%
      mutate(date=ymd(paste0(period, '01')),
             price_usd_per_tonne=value_usd/(value_kg/1000)) %>%
      full_join(prices_monthly, by="date") %>%
      mutate(date = as.Date(date))

    df$commodity[grepl('crude$', df$commodity) & (df$cmd_code %in% oil_codes)] <- "crude_oil"
    df$commodity[grepl('not crude', df$commodity) & (df$cmd_code %in% oil_codes)] <- "oil_products"
    df$commodity[grepl('^Coal.*ovoids', df$commodity) & (df$cmd_code %in% coal_codes)] <- "coal"
    df$commodity[grepl('Coal gas', df$commodity) & (df$cmd_code %in% gas_codes)] <- "coal_gas"
    df$commodity[grepl('gases', df$commodity) & (df$cmd_code %in% gas_codes)] <- "natural_gas"
    df$commodity[df$cmd_code == '271111'] <- "lng"
    df$commodity[df$cmd_code == '271119'] <- "lpg"

    # df$commodity[df$cmd_code %in% fueloils_codes] <- "fuel_oils"
    # df$commodity[df$cmd_code %in% kerosene_codes] <- "kerosene"
    # df$commodity[df$cmd_code %in% diesel_codes] <- "diesel"
    # df$commodity[df$cmd_code %in% gasoline_codes] <- "gasoline"


    df <- df %>%
    filter(grepl('coal$|crude_oil|oil_products|lng|natural_gas|lpg',
                commodity))

    group_cols <- if(is_import){c("reporter_iso3")}else{c("partner_iso3")}
    df <- df %>%
      group_by(across(c("commodity", "date", group_cols))) %>%
      summarise_at(c('value_usd', 'value_kg'), sum, na.rm=T) %>%
      ungroup() %>%
      mutate(price_usd_per_tonne = value_usd/(value_kg/1000))

    if(is_import){
      df$reporter_iso2 <- countrycode::countrycode(df$reporter_iso3, "iso3c", "iso2c", custom_match=c("EUR"="EU"))
      df$reporter <- countrycode::countrycode(df$reporter_iso3, "iso3c", "country.name", custom_match=c("EUR"="EU"))
      df$reporter_iso3 <- NULL
    }else{
      df$partner_iso2 <- countrycode::countrycode(df$partner_iso3, "iso3c", "iso2c")
      df$partner <- countrycode::countrycode(df$partner_iso3, "iso3c", "country.name")
      df$partner_iso3 <- NULL
    }
    return(df)
  }

  imp_cleaned <- clean_comtrade(imp, is_import=T) %>% rename(country=reporter, iso2=reporter_iso2)
  exp_cleaned <- clean_comtrade(exp, is_import=F) %>% rename(country=partner, iso2=partner_iso2)

  eu_iso2s <- utils.get_eu_iso2s()

  # Combine: take source with max flow for that month
  trade <- bind_rows(imp_cleaned, exp_cleaned) %>%
    group_by(commodity, date, country) %>%
    dplyr::slice_max(value_usd, n=1) %>%
    ungroup() %>%
    left_join(prices_monthly)

  eur_usd <- price.eur_per_usd(date_from=min(trade$date),
                               monthly=T)
  trade <- trade %>%
    left_join(eur_usd) %>%
    mutate(value_eur=value_usd * eur_per_usd,
           price_eur_per_tonne=price_usd_per_tonne * eur_per_usd,
    ) %>%
    select(-c(value_usd, price_usd_per_tonne))

  world_price <- trade %>%
    filter(!is.na(price_eur_per_tonne),
           !is.infinite(price_eur_per_tonne)) %>%
    group_by(commodity, date) %>%
    summarise_at(c("value_eur", "value_kg"), sum) %>%
    mutate(world_price_eur_per_tonne = value_eur/(value_kg/1000)) %>%
    select(commodity, date, world_price_eur_per_tonne) %>%
    ungroup()

  trade <- trade %>% left_join(world_price)
  trade$EU <- trade$iso2 %in% eu_iso2s

  trade %>%
    price_models_comtrade.remove_trade_outliers() %>%
    price_models_comtrade.keep_complete_trade() %>%
    arrange(desc(date))
}

price_models_comtrade.remove_trade_outliers <- function(trade){
  trade %>%
    filter((commodity!="natural_gas") | (price_eur_per_tonne < 3000)) %>%
    filter((commodity!="lng") | (price_eur_per_tonne < 1500)) %>%
    filter((commodity!="oil_products") | (
      price_eur_per_tonne > 80 & price_eur_per_tonne < 1500)) %>%
    filter(!is.na(price_eur_per_tonne) & !is.infinite(price_eur_per_tonne)) %>%
    filter(value_kg> 100 * 1000)
}

price_models_comtrade.keep_complete_trade <- function(trade){

  min_share <- 0.3 # Share of respondents for any given date
  eu_iso2s <- utils.get_eu_iso2s()

  # For EU, we only consider whole of EU and assume every reporting is complete enough
  trade <- trade %>%
    filter(!iso2 %in% eu_iso2s)

  max_dates <- trade %>%
    filter(country!='EU') %>%
    filter(year(date)>=2021) %>%
    group_by(commodity, date) %>%
    summarise(count=n()) %>%
    mutate(share=count/max(count)) %>%
    arrange(desc(date)) %>%
    filter(share>min_share) %>%
    distinct(commodity, .keep_all = T) %>%
    ungroup() %>%
    select(commodity, max_date=date)

  trade %>%
    left_join(max_dates) %>%
    filter(country=='EU' | (date < max_date)) %>%
    select(-c(max_date))
}
