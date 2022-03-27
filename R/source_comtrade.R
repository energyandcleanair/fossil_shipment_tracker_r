comtrade.get_flows <- function(use_cache=T){


  f <- "cache/comtrade.RDS"
  if(use_cache && file.exists(f)){ return(readRDS(f)) }

  annual_countries <- c("China","Austria","Denmark","Ireland","Malta")
  years <- seq(2016, 2021)
  codes <- names(hs_commodities)


  # Utils -------------------------------------------------------------------
  exports_as_neg <- function(df) {
    df %>% mutate(netweight_kg=netweight_kg*ifelse(trade_flow=="Export",-1, 1),
                  trade_value_usd=trade_value_usd*ifelse(trade_flow=="Export",-1,1))
  }

  set_date <- function(df){
    df %>%
      mutate(date=as.Date(strptime(paste0(period,"01"), "%Y%m%d")))
  }

  add_commodity <- function(df) {
    df %>%
      filter(commodity_code %in% names(hs_commodities)) %>%
      mutate(commodity=recode(commodity_code, !!!hs_commodities))
  }

  set_reporter_iso <- function(df) {
    df %>%
      mutate(reporter_iso = countrycode(reporter, "country.name", "iso2c", custom_match = c(`EU-28`="EUU")))
  }

  # Collect data ------------------------------------------------------------
  # Most counties have monthly data
  imports_from_russia <- utils.collect_comtrade(partners=c("World", comtradr::ct_country_lookup("Russia")),
                                                reporters="all",
                                                years=years,
                                                frequency="monthly",
                                                codes=c(oil_codes, gas_codes, coal_codes)) %>%
    filter(!reporter %in% annual_countries)

  imports_from_world <- utils.collect_comtrade(partners="World",
                                                reporters="all",
                                                years=years,
                                                frequency="monthly",
                                                codes=c(oil_codes, gas_codes, coal_codes)) %>%
    filter(!reporter %in% annual_countries)

  # But others don't
  import_from_russia_annual <- utils.collect_comtrade(partners=comtradr::ct_country_lookup("Russia"),
                                                     reporters=annual_countries,
                                                     years=seq(2016, 2021),
                                                     frequency="annual",
                                                     codes=c(oil_codes, gas_codes, coal_codes)) %>%
    select(-c(period)) %>%
    left_join(tibble(period=seq(as.Date("2016-01-01"),as.Date("2021-12-31"),by="month")) %>%
                mutate(year=lubridate::year(period),
                       period=as.integer(strftime(period, "%Y%m")))
    ) %>%
    mutate(netweight_kg=netweight_kg/12,
           trade_value_usd=trade_value_usd/12)

  imports_from_russia %<>% bind_rows(import_from_russia_annual)







    q <- comtradr::ct_search(partners = c("Russian Federation"),
                reporters = "All",
                trade_direction = c("imports", "exports"),
                commod_codes=names(codes),
                freq="monthly",
                start_date = year,
                end_date = year) %>%
      mutate(netweight_kg=netweight_kg*ifelse(trade_flow=="Exports",-1, 1),
             trade_value_usd=trade_value_usd*ifelse(trade_flow=="Exports",-1,1)) %>%
      mutate(date=as.Date(strptime(paste0(period,"01"), format="%Y%m%d")))

    if(nrow(q)==0){stop("Comtrade didn't return any row")}

    eur_per_usd <- price.eur_per_usd(date_from=min(q$date, na.rm=T), date_to=max(q$date, na.rm=T)) %>%
      group_by(date=lubridate::floor_date(date, "month")) %>%
      summarise(eur_per_usd=mean(eur_per_usd))

    q %>%
        filter(!grepl("EU-28|ASEAN", reporter)) %>%
        mutate(country_iso2=countrycode(reporter, "country.name", "iso2c")) %>%
        filter(country_iso2 %in% eugb_iso2s) %>%
        mutate(country=countrycode(country_iso2, "iso2c", "country.name")) %>%
        mutate(commodity=unname(codes[commodity_code])) %>%
        group_by(commodity, date, country, partner) %>%
        summarise_at(c("netweight_kg", "trade_value_usd"), sum, na.rm=T) %>%
        mutate(value_tonne=netweight_kg / 1000) %>%
        left_join(eur_per_usd) %>%
        mutate(value_eur=trade_value_usd*eur_per_usd) %>%
        select(country, partner, date, commodity, value_tonne, value_eur) %>%
        tidyr::pivot_longer(c(value_tonne, value_eur), names_to="unit", names_prefix="value_", values_to="value") %>%
        ungroup() %>%
      mutate(source="comtrade")




  saveRDS(q, f)
  return(q)
}




