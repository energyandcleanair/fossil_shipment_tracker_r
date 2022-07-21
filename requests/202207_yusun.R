library(tidyverse)
library(clipr)
library(ggplot2)
library(tidytext)
library(stringr)

voyages <- read_csv('https://api.russiafossiltracker.com/v0/voyage?format=csv')
counter <- read_csv('https://api.russiafossiltracker.com/v0/counter?format=csv')


voyages <- voyages %>%
  filter(commodity_origin_iso2 == 'RU') %>%
  filter(status=='completed')


#-Largest importers by commodity, latest update (CSV file - please see attached image)
data_1 <- counter %>%
  filter(date >= '2022-02-24',
         date <= '2022-06-03') %>%
  mutate(commodity_group=recode(commodity,
                                'lng'='LNG',
                                'natural_gas'='Pipeline gas',
                                 .default=commodity_group)) %>%
  filter(commodity_group != 'gas') %>%
  group_by(destination_country, commodity_group) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
  group_by(commodity_group) %>%
  top_n(10, value_eur) %>%
  arrange(commodity_group, desc(value_eur))

dir.create('results/yusun')
write.csv(data_1, 'results/largest_importing_countries.csv')

data_1 %>%
    mutate(country=reorder_within(destination_country, value_eur, commodity_group)) %>%
    ggplot(aes(value_eur, country, fill=commodity_group)) +
           geom_bar(stat = 'identity') +
          scale_y_reordered() +
           facet_wrap(~ commodity_group, scales="free")


#-Largest importing oil/coal/LNG ports for SK/JP/TW, latest update (CSV file, API file and graphs)
data_2 <- voyages %>%
  filter(destination_iso2 %in% c("KR","JP","TW")) %>%
  group_by(destination_country, destination_iso2, arrival_port_name, commodity_group) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
  filter(!is.na(commodity_group)) %>%
  group_by(commodity_group) %>%
  top_n(10, value_eur) %>%
  arrange(commodity_group, desc(value_eur))


data_2 %>%
  mutate(port=reorder_within(sprintf('%s (%s)', arrival_port_name, destination_iso2), value_eur, commodity_group)) %>%
  ggplot(aes(value_eur, port, fill=commodity_group)) +
  geom_bar(stat = 'identity') +
  scale_y_reordered() +
  facet_wrap(~ commodity_group, scales="free")

write.csv(data_2, 'results/largest_importing_ports.csv')

#-Top importing companies counter for SK/JP/TW (including the monetary valuation of imports from the beginning of invasion to the latest updated date, and which fossil fuels, CSV and API files)
data_3 <- voyages %>%
  filter(destination_iso2 %in% c("KR","JP","TW")) %>%
  group_by(destination_iso2, arrival_berth_owner, commodity_group) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
  filter(!is.na(commodity_group)) %>%
  group_by(commodity_group) %>%
  top_n(10, value_eur) %>%
  arrange(commodity_group, desc(value_eur)) %>%
  mutate(arrival_berth_owner=tidyr::replace_na(arrival_berth_owner, "Unknown"))


data_3 %>%
  mutate(company=reorder_within(str_wrap(sprintf('%s (%s)', arrival_berth_owner, destination_iso2), 20), value_eur, commodity_group)) %>%
  ggplot(aes(value_eur, company, fill=commodity_group)) +
  geom_bar(stat = 'identity') +
  scale_y_reordered() +
  facet_wrap(~ commodity_group, scales="free")

write.csv(data_3, 'results/largest_importing_companies.csv')

#-How many euros worth of Russian crude oil transported from South Korea to Chinese tankers/China, latest calculation
data_4 <- voyages %>%
  filter(destination_iso2=='KR',
         commodity_destination_iso2=='CN') %>%
  group_by(commodity) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T)

write.csv(data_4, 'results/south_korea_to_china.csv')

#-Counter for SK/JP/TW fossil fuel imports (monetary valuation) and how much of that goes to Russian federal budget, latest update








# Old exploration ---------------------------------------------------------

voyages %>%
  filter(strftime(floor_date(arrival_date_utc, 'month') , '%Y-%m-%d')== '2022-06-01',
         commodity_origin_iso2 == 'RU') %>%
  group_by(arrival_port_name, destination_country, commodity) %>%
  summarise(value_mn_eur=sum(value_eur, na.rm=T) / 1e6) %>%
  arrange(desc(value_mn_eur))%>%
  filter(grepl('yeosu', arrival_port_name, T))


voyages %>%
  filter(
    # strftime(floor_date(arrival_date_utc, 'month') , '%Y-%m-%d')== '2022-06-01',
         commodity_origin_iso2 == 'RU') %>%
  group_by(arrival_port_name, destination_country, commodity_destination_country, arrival_berth_name, commodity) %>%
  summarise(value_mn_eur=sum(value_eur, na.rm=T) / 1e6) %>%
  arrange(desc(value_mn_eur)) %>%
  filter(destination_country == 'South Korea') %>%
  View()
  # filter(grepl('yeosu', arrival_port_name, T))


# Largest crude-oil importing ports









voyages %>%
  filter(
    arrival_date_utc >= '2022-02-24',
    arrival_date_utc <= '2022-06-03',
    commodity_origin_iso2 == 'RU',
    commodity %in% c('oil_products', 'lng', 'crude_oil', 'coal')
    ) %>%
  group_by(commodity, destination_country, arrival_port_name) %>%
  summarise(value_mn_eur=round(sum(value_eur, na.rm=T) / 1e6),
            kt=round(sum(value_tonne)/1000),
            count=n(),
            companies = paste(unique(setdiff(arrival_berth_owner, NA)), collapse="; "),
            latest_date = lubridate::floor_date(max(arrival_date_utc), 'day')
            ) %>%
  arrange(desc(value_mn_eur)) %>%
  group_by(commodity) %>%
  top_n(10, value_mn_eur) %>%
  arrange(commodity)


clipr::write_last_clip()

voyages %>%
  filter(
    arrival_date_utc >= '2022-02-24',
    arrival_date_utc <= '2022-06-03',
    # commodity_origin_iso2 == 'RU',
    departure_iso2 == 'RU',
    destination_iso2 %in% c('JP', 'KR', 'TW'),
    commodity %in% c('oil_products', 'lng', 'crude_oil', 'coal')
  ) %>%
  group_by(arrival_berth_owner) %>%
  summarise(value_mn_eur=round(sum(value_eur, na.rm=T) / 1e6)) %>%
  arrange(desc(value_mn_eur))

clipr::write_last_clip()
