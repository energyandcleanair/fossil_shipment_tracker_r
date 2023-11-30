price_models_eurostat.get_predicted <- function(production=F){

  prices_daily <- get_prices_daily(running_days=30) %>%
    arrange(desc(date)) %>%
    filter(!is.na(date))

  #Note: models where trained with datetime, so need to keep it as datetime
  if(production){
    suffix=''
  }else{
    suffix='_development'
  }

  models <- readRDS(system.file("extdata", sprintf("pricing_models_eurostat%s.RDS", suffix), package="russiacounter"))

  prices <- models %>%
    mutate(new_data=list(prices_daily)) %>%
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

  p <- prices %>%
    dplyr::select(date,
                  commodity,
                  eur_per_tonne=price_eur_per_tonne) %>%
    mutate(date=as.Date(date),
           country_iso2=NA)

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
    select(-c(country_iso2)) %>%

    # to be compatible with kpler product ids on Python side
    mutate(commodity = paste0("kpler_",tolower(gsub(" |/","_",commodity))))


  return(p_formatted)
}


price_models_eurostat.build <- function(production=F, refresh_trade=T, diagnostic_folder='diagnostics'){

  #TODO Clean that up
  library(tidyverse)
  library(magrittr)
  library(lubridate)
  library(countrycode)


  # Collect predictors ------------------------------------------------------
  prices_monthly <- get_prices_monthly()

  # Collect trade data ----------------------------------------------------------
  trade_raw <- price_models_eurostat.get_trade()

  trade <- trade_raw %>%
    select(date, commodity=product, price_eur_per_tonne) %>%
    left_join(prices_monthly %>%
                mutate(date=as.Date(date)))


  trade_with_predictions <- trade %>%
    group_by(commodity) %>%
    group_map(function(df, group) {
      print(group$commodity)
      start_year = 2015 #ifelse(group$commodity=='natural_gas', 2016, ifelse(group$commodity=='lng', 2018, 2015))
      independents = case_when(T ~ 'brent + lag(brent, 1) + refining_light + refining_medium + refining_heavy + lag(refining_light,1)  + lag(refining_medium,1)  + lag(refining_heavy,1)')

      df <- df %>% arrange(date) %>% filter(year(date)>=start_year)
      m <- df %>%
        lm(as.formula(paste('price_eur_per_tonne ~', independents)), data=.)

      tibble_row(data=list(as.data.frame(df %>% mutate(predicted_price = predict(m, df)))),
                 model=list(m),
                 commodity=group$commodity,
                 price_ceiling = max(df$price_eur_per_tonne),
                 price_floor = min(df$price_eur_per_tonne))
    }) %>% do.call(bind_rows, .)


  if(!is.null(diagnostic_folder)){
    plt <- trade_with_predictions %>%
      select(-c(model)) %>%
      tidyr::unnest(data)  %>%
      pivot_longer(c(price_eur_per_tonne, predicted_price, price_ceiling)) %>%
      filter(year(date)>=2016) %>%
      ggplot(aes(date, value, col=name)) +
      facet_wrap(~commodity) + geom_line() +
      scale_y_continuous(limits=c(0, NA))

    ggsave(file.path(diagnostic_folder, 'pricing_model_eurostat.png'), plot=plt)

    plt <- trade_with_predictions %>% select(-c(model)) %>%
      tidyr::unnest(data)  %>% filter(year(date)>=2016) %>%
      ggplot(aes(price_eur_per_tonne, predicted_price)) +
      facet_wrap(~commodity, scales='free') + geom_point() + geom_abline() + geom_smooth()
    ggsave(file.path(diagnostic_folder, 'pricing_model_eurostat_diagnostics.png'), plot=plt)
  }

  suffix <- ifelse(production, '', '_development')
  saveRDS(trade_with_predictions, sprintf('inst/extdata/pricing_models_eurostat%s.RDS',suffix))
  saveRDS(trade_with_predictions, sprintf('data/pricing_models_eurostat%s.RDS',suffix))
}

price_models_eurostat.build_url <- function(countries, commodities, date_from) {

  # concatenate country codes with "+" separator
  country_str <- paste0(countries, collapse = "+")

  # concatenate commodity codes with "+" separator
  commodity_str <- paste0(commodities, collapse = "+")

  # construct the URL string
  url <- paste0("https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1/data/DS-045409/M.EU27_2020+",
                country_str,
                ".EU27_2020_EXTRA.",
                commodity_str,
                ".1+2.VALUE_IN_EUROS+QUANTITY_IN_100KG/?format=SDMX-CSV&startPeriod=",
                date_from,
                # "&endPeriod=",
                # end_date,
                "&lang=en&label=both")
  return(url)
}

price_models_eurostat.get_trade <- function(date_from='2015-01-01'){


  fueloils_codes <- c(27101951, 27101962, 27101966, 27101967)
  kerosene_codes <- c(27101921, 27101925)
  diesel_codes <- c(27101931, 27101935, 27101943, 27101946, 27101947, 27101948,
                    27102011, 27102016, 27102019)
  gasoline_codes <- c(271012)


  commodity_tbl <- tribble(
    ~product, ~code,

    "Gasoline", gasoline_codes,
    "Gasoline/Naphtha", gasoline_codes,
    "Naphtha", gasoline_codes,
    "Blending Comps", gasoline_codes,

    # TODO add more commodities that are covered by Blendings (e.g. )
    "Blendings", gasoline_codes,

    "Diesel", diesel_codes,
    "Gasoil", diesel_codes,
    "Gasoil/Diesel", diesel_codes,
    "VGO", diesel_codes,

    "Jet", c(27101921),
    "Kero/Jet", c(27101921, 27101925),
    "Kerosene", c(27101925),

    "Fuel Oils", fueloils_codes,
    "FO", fueloils_codes,
    "SRFO", fueloils_codes,
    "Slurry", fueloils_codes,
  ) %>%
    tidyr::unnest(code)

  countries <- c("EU")
  url <- price_models_eurostat.build_url(countries, unique(commodity_tbl$code), date_from)
  trade_raw <- read_csv(url)

  to_code <- function(x){sub("\\:.*", "", x)}
  to_label <- function(x){sub(".*\\:", "", x)}
  to_date <- function(period){as.Date(paste0(period, "-01"))}
  trade <-  trade_raw %>%
    select(date=TIME_PERIOD,
           reporter,
           partner,
           code=product,
           flow,
           unit=indicators,
           value=OBS_VALUE) %>%
    mutate_at(c('reporter', 'partner', 'code','unit'), to_code) %>%
    mutate_at(c('flow'), to_label) %>%
    mutate_at(c('date'), to_date) %>%
    filter(flow=='IMPORT') %>%
    group_by(date, partner, code, flow, unit) %>%
    summarise(value = sum(value))

  # join with commodity table, using the most specific code
  commodity_tbl %>%
    mutate_at("code", as.character) %>%
    inner_join(trade, multiple='all') %>%
    group_by(product, date, unit) %>%
    summarise(value=sum(value)) %>%
    ungroup() %>%
    tidyr::spread(unit, value) %>%
    mutate(price_eur_per_tonne = VALUE_IN_EUROS / QUANTITY_IN_100KG * 10)

}

price_models_eurostat.identify_codes <- function(){


  # Gasoline
  gasoline <- c(27101241, 27101245, 27101249)
  url <- price_models_eurostat.build_url(countries="EU", commodities=gasoline, "2022-01-01")
  trade <- read_csv(url) %>%
    group_by(product, indicators) %>%
    summarise(value=sum(OBS_VALUE)) %>%
    spread(indicators, value) %>%
    mutate(price=`VALUE_IN_EUROS:VALUE_IN_EUROS` / `QUANTITY_IN_100KG:QUANTITY_IN_100KG`*10) %>%
    arrange(desc(`QUANTITY_IN_100KG:QUANTITY_IN_100KG`))

  # Diesel choices
  diesel <- c(27101931, 27101932, 27101935, 27101943, 27101946, 27101947, 27101948)
  url <- price_models_eurostat.build_url(countries="EU", commodities=diesel, "2022-01-01")
  trade <- read_csv(url) %>%
    group_by(product, indicators) %>%
    summarise(value=sum(OBS_VALUE)) %>%
    spread(indicators, value) %>%
    mutate(price=`VALUE_IN_EUROS:VALUE_IN_EUROS` / `QUANTITY_IN_100KG:QUANTITY_IN_100KG`*10) %>%
    arrange(desc(`QUANTITY_IN_100KG:QUANTITY_IN_100KG`))

  # Kerosene
  kerosene <- c(27101921, 27101925)
  url <- price_models_eurostat.build_url(countries="EU", commodities=kerosene, "2022-01-01")
  trade <- read_csv(url) %>%
    group_by(product, indicators) %>%
    summarise(value=sum(OBS_VALUE)) %>%
    spread(indicators, value) %>%
    mutate(price=`VALUE_IN_EUROS:VALUE_IN_EUROS` / `QUANTITY_IN_100KG:QUANTITY_IN_100KG`*10) %>%
    arrange(desc(`QUANTITY_IN_100KG:QUANTITY_IN_100KG`))

  # Fuel oils
  fueloils <- c(27101951, 27101955, 27101962, 27101966, 27101967)
  url <- price_models_eurostat.build_url(countries="EU", commodities=fueloils, "2022-01-01")
  trade <- read_csv(url) %>%
    group_by(product, indicators) %>%
    summarise(value=sum(OBS_VALUE)) %>%
    spread(indicators, value) %>%
    mutate(price=`VALUE_IN_EUROS:VALUE_IN_EUROS` / `QUANTITY_IN_100KG:QUANTITY_IN_100KG`*10) %>%
    arrange(desc(`QUANTITY_IN_100KG:QUANTITY_IN_100KG`))
}
