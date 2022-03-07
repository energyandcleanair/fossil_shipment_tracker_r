source('price.R')
source('comtrade.R')
source('entsog.R')
source('iea.R')

entso <- get_entsog()
comtrade <- get_comtrade()
eurostat <- get_eurostat()
iea <- get_iea()

price <- get_gas_price() %>%
  group_by(date=lubridate::floor_date(date, 'month')) %>%
  summarise_at("price", mean, na.rm=T)


operators <- entsog::operators()

# Comparing volume and prices from different sources
d <- bind_rows(
  entso %>%
    group_by(date=lubridate::floor_date(date,"month"), country=reporter) %>%
    summarise(value_MWh=sum(value_MWh, na.rm=T)) %>%
    left_join(price) %>%
    mutate(value_EUR=value_MWh*price,
           source="ENTOSG * TTF"),

  comtrade %>%
    ungroup() %>%
    filter(grepl("Petroleum gases", commodity)) %>%
    select(country=reporter, date, value_MWh, value_EUR) %>%
    mutate(source="Comtrade"),


  eurostat %>%
    ungroup() %>%
    filter(grepl("natural gas", product)) %>%
    filter(unit=="MWh") %>%
    group_by(date=lubridate::floor_date(time,"month"), country) %>%
    summarise(value_MWh=sum(value, na.rm=T)) %>%
    ungroup() %>%
    mutate(value_EUR=0, source="Eurostat"),

  iea



) %>%
  tidyr::pivot_longer(cols=c(value_EUR, value_MWh),
                      names_to="unit",
                      names_prefix="value_") %>%
  filter(country %in% unique(c(entso$repoter, comtrade$reporter)))


ggplot(d) +
  geom_bar(aes(date, value, fill=source), stat="identity", position="dodge") +
  facet_wrap(unit~country, scales="free")
