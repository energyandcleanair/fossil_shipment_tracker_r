eurostat_exeu.get_flows <- function(use_cache=F){

  # f <- 'data/DS-1262527_1_Data.csv'
  f <- system.file("extdata", "DS-1262527_1_Data.RDS", package="russiacounter")
  print(sprintf("Reading %s",f))
  if(!file.exists(f)){
    stop(sprintf("Can't find file %s",f))
  }

  trade <- readRDS(f) %>%
    mutate(date=paste(1,PERIOD) %>% strptime("%d %b. %Y") %>% as.Date(),
           Value = gsub(',', '', Value) %>% as.numeric) %>%
    filter(!is.na(date),
           date >= "2015-01-01") %>%
    tidyr::spread(INDICATORS, Value) %>%
    select(date, country=REPORTER, partner=PARTNER, direction=FLOW, commodity=PRODUCT, transport=TRANSPORT_MODE,
           value_tonne = QUANTITY_IN_TONS, value_eur = VALUE_IN_EUROS) %>%
    mutate(factor=ifelse(direction=="IMPORT",1,-1),
           value_tonne=value_tonne*factor,
           value_eur=value_eur*factor
           ) %>%
    group_by(date, country, partner, commodity, transport) %>%
    summarise(value_tonne=sum(value_tonne, na.rm=T),
              value_eur=sum(value_eur, na.rm=T)) %>%
    mutate(value_eur_tonne = value_eur/value_tonne) %>%
    tidyr::pivot_longer(cols=c(value_eur, value_tonne, value_eur_tonne),
                        names_to="unit",
                        names_prefix="value_")

  trade$commodity[grep('crude$', trade$commodity)] <- "crude_oil"
  trade$commodity[grep('excl\\. crude', trade$commodity)] <- "oil_products"
  trade$commodity[grep('^Coal', trade$commodity)] <- "coal"
  trade$commodity[grep('^Coke', trade$commodity)] <- "coke"
  trade$commodity[grepl('gas', trade$commodity) & grepl('Fixed', trade$transport)] <- "natural_gas"
  trade$commodity[grepl('gas', trade$commodity) & grepl('Sea', trade$transport)] <- "lng"


  # trade$transport[grep('Fixed', trade$transport)] <- "pipeline"
  # trade$transport[grep('Sea', trade$transport)] <- "sea"

  trade <- trade %>%
    filter(grepl('^natural_gas|^coal|crude_oil|oil_products|lng|^coke', commodity),
           !is.na(value)) %>%
    ungroup()

  # Project in the future
  # [Feb-Mar 2021 average] * (1 + [Nov-Dec 2021 YoY change])
  ratios <- trade %>%
    ungroup() %>%
    mutate(year=lubridate::year(date)) %>%
    filter(year %in% c(2020, 2021),
           month(date) %in% c(11,12)) %>%
    group_by(year, country, partner, commodity, unit) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    tidyr::pivot_wider(names_from=year, values_from=value, names_prefix="value_") %>%
    mutate(ratio=value_2021/value_2020) %>%
    # since there is a reported 1/3 drop in seaborne cargoes
    # I would assume that that's 1/2 for Europe, and apply that reduction to all seaborne imports
    mutate(ratio=ratio*ifelse(grepl("lng", commodity), 0.5, 1))


  trade_future <- trade %>%
    ungroup() %>%
    mutate(year=lubridate::year(date)) %>%
    filter(year %in% c(2021),
           month(date) %in% c(1, 2, 3)) %>%
    left_join(ratios) %>%
    mutate(date=date+lubridate::years(1),
           value=value*ratio) %>%
    select(-c(value_2020, value_2021, ratio))

  trade <- bind_rows(trade, trade_future)
  trade$source <- "eurostat_exeu"
  return(trade)
}
