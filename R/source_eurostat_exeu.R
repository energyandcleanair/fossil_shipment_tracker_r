eurostat_exeu.get_flows <- function(use_cache=F){

  trade <- read_csv('data/DS-1262527_1_Data.csv') %>%
    mutate(date=paste(1,PERIOD) %>% strptime("%d %b. %Y") %>% as.Date(),
           Value = gsub(',', '', Value) %>% as.numeric) %>%
    filter(!is.na(date)) %>%
    # filter(INDICATORS=="QUANTITY_IN_TONS") %>%
    spread(INDICATORS, Value) %>%
    select(date, country=REPORTER, partner=PARTNER, direction=FLOW, commodity=PRODUCT, transport=TRANSPORT_MODE,
           value = QUANTITY_IN_TONS, eur = VALUE_IN_EUROS) %>%
    mutate(price = eur/value) %>%
    select(-c(eur)) %>%
    mutate(value=value*ifelse(direction=="IMPORT",1,-1)) %>%
    select(-c(direction)) %>%
    group_by(date, country, partner, commodity, transport) %>%
    summarise(value=sum(value, na.rm=T),
              price=mean(price, na.rm=T)) %>%
    ungroup() %>%
    filter(date >= "2015-01-01")

  trade$commodity[grep('crude$', trade$commodity)] <- "crude_oil"
  trade$commodity[grep('excl\\. crude', trade$commodity)] <- "oil_products"
  trade$commodity[grep('^Coal', trade$commodity)] <- "coal"
  trade$commodity[grep('gas', trade$commodity)] <- "natural_gas"
  trade$transport[grep('Fixed', trade$transport)] <- "pipeline"
  trade$transport[grep('Sea', trade$transport)] <- "sea"

  trade %<>% filter(grepl('pipeline|sea', transport),
                    grepl('^natural_gas|^coal|crude_oil|oil_products', commodity),
                    !is.na(value))

  # Project in the future
  # [Feb-Mar 2021 average] * (1 + [Nov-Dec 2021 YoY change])
  ratios <- trade %>%
    ungroup() %>%
    mutate(year=lubridate::year(date)) %>%
    filter(year %in% c(2020, 2021),
           month(date) %in% c(11,12)) %>%
    group_by(year, country, partner, commodity, transport) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    tidyr::pivot_wider(names_from=year, values_from=value, names_prefix="value_") %>%
    mutate(ratio=value_2021/value_2020) %>%
    # since there is a reported 1/3 drop in seaborne cargoes
    # I would assume that that's 1/2 for Europe, and apply that reduction to all seaborne imports
    mutate(ratio=ratio*ifelse(grepl("Sea", transport), 0.5, 1))


  trade_future <- trade %>%
    ungroup() %>%
    mutate(year=lubridate::year(date)) %>%
    filter(year %in% c(2021),
           month(date) %in% c(2,3)) %>%
    left_join(ratios) %>%
    mutate(date=date+lubridate::years(1),
           value=value*ratio) %>%
    select(-c(value_2020, value_2021, ratio))

  trade <- bind_rows(trade, trade_future)
  trade$unit <- "tonne"
  trade$unit_price <- "EUR/tonne"
  trade$source <- "eurostat_exeu"
  return(trade)
}
