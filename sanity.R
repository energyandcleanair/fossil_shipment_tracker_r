source('constants.R')
source('price.R')
source('comtrade.R')
source('entsog.R')
source('iea.R')

# price <- get_gas_price() %>%
#   group_by(date=lubridate::floor_date(date, 'month')) %>%
#   summarise_at("price", mean, na.rm=T)


entso <- db.download_flows("entsog")
comtrade <- db.download_flows("comtrade")
eurostat <- db.download_flows("eurostat")
eurostat_byhs <- db.download_flows("eurostat_byhs")
eurostat_exeu <- db.download_flows("eurostat_exeu")


d <- bind_rows(
  entso,
  comtrade,
  eurostat,
  eurostat_byhs,
  eurostat_exeu
) %>%
  group_by(country, partner, commodity, unit, source, date=lubridate::floor_date(date, 'month')) %>%
  summarise(value=sum(value, na.rm=T)) %>%
  mutate(country=ifelse(grepl("Russia", country),"Russia", country),
         partner=ifelse(grepl("Russia", partner),"Russia", partner))

unique(d$partner)

d %>% filter(country %in% c("Germany"),
             grepl("gas", commodity)) %>%
  ggplot() +
  geom_line(aes(date, value, col=source)) +
  facet_wrap(country~partner+unit+commodity)



entso <- get_entsog()
comtrade <- get_comtrade()
eurostat <- get_eurostat()
iea <- get_iea()

# operators <- entsog::operators()

# Comparing volume and prices from different sources
d <- bind_rows(
  entso %>%
    filter(unit=="MWh/day") %>%
    group_by(date=lubridate::floor_date(date,"month"), country, commodity, source) %>%
    summarise(value=sum(value, na.rm=T),
              unit="MWh/month"),
#
#   comtrade %>%
#     filter(grepl("Petroleum gases", commodity),
#            unit=="MWh/month") %>%
#     select(country, date, value, unit, commodity) %>%
#     mutate(source="Comtrade"),

  iea %>%
    filter(unit=="MWh/month") %>%
    select(country, date=month, value, unit, source, commodity),

  eurostat %>%
    filter(unit=="MWh/month") %>%
    filter(value>0) %>%
    # filter(!is.na(country)) %>%
    # filter(country %in% unique(c(comtrade$country, iea$country, entso$country))) %>%
    select(country, date, value, unit, source)

) %>%
  mutate(value=value/1e6,
         unit="TWh/month")


ggplot(d %>% filter(date>="2017-01-01")) +
  geom_bar(aes(date, value, fill=source), stat="identity", position="dodge") +
  facet_wrap(~country, scales="free")


ggplot(d %>%
         # filter(country!="Ukraine") %>%
         group_by(source, date=lubridate::floor_date(date, "year")) %>%
         summarise(value=sum(value, na.rm=T)) %>%
         filter(date>="2017-01-01")) +
  geom_bar(aes(date, value, fill=source), stat="identity", position="dodge")
