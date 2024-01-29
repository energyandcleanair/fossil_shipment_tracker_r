eurostat_byhs.get_flows <- function(use_cache=T){


  f_cache <- "cache/eurostat_byhs.RDS"
  if(use_cache && file.exists(f_cache)){ return(readRDS(f_cache)) }

  # f <- 'data/data-16870525.csv'
  # read_csv(f) %>% saveRDS("inst/extdata/data-16870525.RDS")

  f <- system.file("extdata", "data-16870525.RDS", package="russiacounter")
  log_info("Reading {f}")
  if(!file.exists(f)){
    stop(sprintf("Can't find file %s",f))
  }

  trade <- readRDS(f) %>%
    mutate(date=paste(PERIOD,"01") %>% strptime("%Y%m%d") %>% as.Date()) %>%
    select(date, country_iso2=REPORTER, partner_iso2=PARTNER,
           commodity_code=PRODUCT, commodity=PRODUCT_LAB,
           direction=FLOW_LAB, unit=INDICATORS_LAB, value = INDICATOR_VALUE) %>%
    filter(!is.na(date))

  # Only keep countries with iso2
  trade <- trade %>%
    mutate(country=countrycode(country_iso2, "iso2c", "country.name"),
           partner=countrycode(partner_iso2, "iso2c", "country.name")) %>%
    filter(!is.na(partner),
           !is.na(country)) %>%
    mutate(value=value * ifelse(unit=="QUANTITY_IN_100KG", 0.1, 1),
           unit=recode(unit, QUANTITY_IN_100KG="tonne", VALUE_IN_EUROS="eur")) %>%
    mutate(commodity=recode(commodity_code, !!!hs_commodities)) %>%
    mutate(value=value*ifelse(direction=="IMPORT",1,-1)) %>%
    group_by(date, country, partner, commodity, unit) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    ungroup() %>%
    mutate(source="eurostat_byhs")

  saveRDS(trade, f_cache)
  return(trade)
}




