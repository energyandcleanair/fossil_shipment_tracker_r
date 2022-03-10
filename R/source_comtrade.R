comtrade.get_flows <- function(use_cache=T){


  f <- "cache/comtrade.RDS"
  if(use_cache && file.exists(f)){ return(readRDS(f)) }

  get_year_data <- function(year){
    print(year)
    # usd_per_eur <- getSymbols("EUR=X", from=paste0(year,"-01-01"), to= paste0(year,"-12-31"), auto.assign = F)
    # usd_per_eur <- tibble(date=index(usd_per_eur), value=as.numeric(usd_per_eur$`EUR=X.Adjusted`)) %>%
    #   group_by(date=lubridate::floor_date(date, 'month')) %>%
    #   summarise(usd_per_eur=mean(value, na.rm=T))

    # oil_codes <- c("2709","2710")
    gas_codes <- c("2711","271121","271111")
    # coal_codes <- c("2701","2704","2705","2706")

    # countries <- c("Germany", "Poland", "Finland", "Ukraine", "Estonia", "Belarus", "Lithuania", "Latvia", "Estonia", "Turkey", "Georgia")
    # countries_chunks <- split(countries, rep(1:ceiling(length(countries)/5), each=5)[1:length(countries)])

    q <- ct_search(partners = "Russian Federation",
                reporters = "All",
                trade_direction = c("imports", "exports"),
                commod_codes=gas_codes,
                freq="monthly",
                start_date = year,
                end_date = year) %>%
      mutate(netweight_kg=netweight_kg*ifelse(trade_flow=="Exports",-1, 1),
             trade_value_usd=trade_value_usd*ifelse(trade_flow=="Exports",-1,1))

    if(nrow(q)==0){return(NULL)}

    q %>%
        filter(!grepl("EU-28", reporter)) %>%
        mutate(country_iso2=countrycode(reporter, "country.name", "iso2c")) %>%
        filter(country_iso2 %in% eugb_iso2s) %>%
        mutate(country=countrycode(country_iso2, "iso2c", "country.name")) %>%
        mutate(date=as.Date(strptime(paste0(period,"01"), format="%Y%m%d"))) %>%
        group_by(commodity_code, commodity, date, country, partner) %>%
        summarise_at(c("netweight_kg", "trade_value_usd"), sum, na.rm=T) %>%
        mutate(value=netweight_kg / kg_per_m3 * gcv_MWh_per_m3,
               unit="MWh/month") %>%
        # left_join(usd_per_eur) %>%
        # mutate(value_EUR=trade_value_usd/usd_per_eur) %>%
        select(country, date, commodity, value, unit, partner, value_usd=trade_value_usd) %>%
        ungroup()

  }

  q <- lapply(seq(2020, 2022), get_year_data) %>%
    do.call(bind_rows, .) %>%
    mutate(source="Comtrade")
  saveRDS(q, f)
  return(q)
}




