eurostat_exeu.get_flows <- function(use_cache=T){

  f_cache <- "cache/eurostat_exeu.RDS"
  if(use_cache & file.exists(f_cache)){
    return(readRDS(f_cache))
  }

  f <- system.file("extdata", "DS-1262527_1_Data_20220316.RDS", package="russiacounter")
  f <- "inst/extdata/DS-1262527_1_Data_20220424.RDS"
  print(sprintf("Reading %s",f))
  if(!file.exists(f)){
    stop(sprintf("Can't find file %s",f))
  }

  # read_csv(f) %>%
  #   mutate(partner=ifelse(grepl("Russia", partner), "Russia", "World")) %>%
  #   saveRDS(f)

  trade <- readRDS(f)

  trade <- trade %>%
    mutate(date=paste(1,PERIOD) %>% strptime("%d %b. %Y") %>% as.Date(),
           Value = gsub(',| ', '', Value) %>% as.numeric,) %>%
    filter(!is.na(date),
           date >= "2015-01-01") %>%
    tidyr::spread(INDICATORS, Value) %>%
    select(date, iso2=REPORTER, partner=PARTNER, direction=FLOW, commodity_code=PRODUCT, transport=TRANSPORT_MODE,
           value_tonne = QUANTITY_IN_TONS, value_eur = VALUE_IN_EUROS) %>%
      mutate(factor=ifelse(direction=="IMPORT",1,-1),
           value_tonne=value_tonne*factor,
           value_eur=value_eur*factor
           ) %>%
    group_by(date,
             iso2,
             country=countrycode::countrycode(iso2, "iso2c", "country.name", custom_match=c("EU"="EU")),
             partner, commodity_code, transport) %>%
    summarise(value_tonne=sum(value_tonne, na.rm=T),
              value_eur=sum(value_eur, na.rm=T)) %>%
    mutate(value_eur_tonne = value_eur/value_tonne) %>%
    tidyr::pivot_longer(cols=c(value_eur, value_tonne, value_eur_tonne),
                        names_to="unit",
                        names_prefix="value_") %>%
    mutate(value=tidyr::replace_na(value, 0))


  trade <- trade %>%
    filter(commodity_code %in% names(hs_commodities)) %>%
    mutate(commodity=recode(commodity_code, !!!hs_commodities))

  # trade %>% ungroup() %>%
  #   filter(value>0, unit=="tonne") %>%
  #   group_by(country, commodity, unit) %>% summarise(value=sum(value)) %>%
  #   tidyr::spread(commodity, value, fill = 0) %>%
  #   mutate(gas_all2=lng+natural_gas) %>%
  #   select(country, unit, gas_all, gas_all2)

  # Some countries only share total gas numbers
  # trade %>% ungroup() %>% filter(value>0, unit=="tonne") %>%
  #   group_by(country, commodity, transport, unit) %>% summarise(value=sum(value)) %>%
  #   tidyr::spread(commodity, value, fill = 0) %>%
  #   mutate(gas_all2=lng+natural_gas) %>%
  #   select(country, unit, transport, gas_all, gas_all2) %>% View()

  # Clean commodities
  trade$transport[grep('Fixed', trade$transport)] <- "pipeline"
  trade$transport[grep('Sea', trade$transport)] <- "seaborne"
  trade$transport[grep('Rail|Road', trade$transport)] <- "rail_road"

  # We ignore others (#TODO investigate a bit further Inland Waterway)
  trade$transport[!grep('pipeline|seaborne|rail_road', trade$transport)] <- "other"

  trade <- trade %>%
    filter(grepl('^natural_gas|gas_all|^coal|oil|oil_products|lng|^coke', commodity),
           !is.na(value)) %>%
    ungroup()


  trade$source <- "eurostat_exeu"
  saveRDS(trade, f_cache)
  return(trade)
}
