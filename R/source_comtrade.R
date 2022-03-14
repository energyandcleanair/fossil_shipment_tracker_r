comtrade.get_flows <- function(use_cache=T){


  f <- "cache/comtrade.RDS"
  if(use_cache && file.exists(f)){ return(readRDS(f)) }

  get_year_data <- function(year){
    print(year)
    Sys.sleep(30)

    oil_codes <- c("2709"="crude_oil", "2710"="oil_others")
    gas_codes <- c("2711"="gas_all","271121"="natural_gas","271111"="lng")
    coal_codes <- c("2701"="coal","2704"="coke")
    codes <- c(oil_codes, gas_codes, coal_codes)
    # countries <- c("Germany", "Poland", "Finland", "Ukraine", "Estonia", "Belarus", "Lithuania", "Latvia", "Estonia", "Turkey", "Georgia")
    # countries_chunks <- split(countries, rep(1:ceiling(length(countries)/5), each=5)[1:length(countries)])

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

  }

  q <- lapply(seq(2020, 2021), get_year_data) %>%
    do.call(bind_rows, .) %>%
    mutate(source="Comtrade")
  saveRDS(q, f)
  return(q)
}




