source('R/source_kpler.R')
library(countrycode)
library(remotes)
install_github('energyandcleanair/creahelpers')
install_github('energyandcleanair/rcrea')

library(rcrea)

flows_kpler <- kpler.get_flows(F)
flows_crea <- read_csv(sprintf('https://api.russiafossiltracker.com/v0/voyage?aggregate_by=commodity_origin_iso2,commodity_destination_iso2,commodity,date&format=csv&date_from=%s&commodity=crude_oil,oil_products,oil_or_chemical', min(flows_kpler$date)))

flows_kpler_fmt <- flows_kpler %>%
  mutate(commodity_origin_iso2 = countrycode(origin_country, "country.name", "iso2c"),
         commodity_destination_iso2 = countrycode(destination_country, "country.name", "iso2c", custom_match = c("Unknown"="Unknown")),
  ) %>%
  filter(value_tonne>0,
         !is.na(commodity_destination_iso2)) %>%
  select(commodity_origin_iso2, commodity_destination_iso2, date, commodity=product, value_tonne) %>%
  mutate(source='Kpler')

flows_crea_fmt <- flows_crea %>%
  select(commodity_origin_iso2, commodity_destination_iso2, date=departure_date, commodity, value_tonne) %>%
  mutate(source='CREA')


# Compare Crude totals
bind_rows(flows_kpler_fmt, flows_crea_fmt) %>%
  filter(date >= lubridate::ceiling_date(min(flows_kpler$date), "month"),
         date <= '2022-12-31') %>%
  filter(grepl('crude', commodity, ignore.case=T)) %>%
  filter(commodity_origin_iso2 != 'KZ') %>%
  filter(commodity != 'Crude/Co') %>%
  group_by(commodity_origin_iso2, commodity, source, month=floor_date(date, 'month'), commodity) %>%
  summarise(value_tonne=sum(value_tonne, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(commodity_origin_iso2, commodity, source, month, fill=list(value_tonne=0)) %>%
  ggplot() +
  geom_bar(aes(month, value_tonne / 1e6, fill=source), stat='identity',
           position='dodge') +
  facet_wrap(~commodity_origin_iso2, scale='free_y') +
  rcrea::theme_crea() +
  labs(title='2022 Crude oil export tonnage from selected countries',
       caption='Shipments only.',
       y='million tonnes / month',
       x=NULL) +
  rcrea::scale_fill_crea_d() +
  scale_x_date(date_labels = "%b",
               date_breaks = '1 month')



# Compare Products totals
bind_rows(flows_kpler_fmt, flows_crea_fmt) %>%
  filter(date >= lubridate::ceiling_date(min(flows_kpler$date), "month"),
         date <= '2022-12-31') %>%


  # Play here with filters to choose commodities

  filter(!grepl('crude', commodity, ignore.case=T) | commodity=='crude_oilZZZZ') %>%
  filter(!grepl('Naphta|Others|Clean|Oils', commodity)) %>%


  filter(commodity_origin_iso2 != 'KZ') %>%
  group_by(commodity_origin_iso2, commodity, source, month=floor_date(date, 'month'), commodity) %>%
  summarise(value_tonne=sum(value_tonne, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(commodity_origin_iso2, commodity, source, month, fill=list(value_tonne=0)) %>%
  ggplot() +
  geom_bar(aes(month, value_tonne / 1e6, fill=commodity), stat='identity') +
  facet_grid(commodity_origin_iso2 ~ source, scale='free_y') +
  rcrea::theme_crea() +
  labs(title='2022 Non crude oil export tonnage from selected countries',
       caption='Shipments only.',
       y='million tonnes / month',
       x=NULL) +
  rcrea::scale_fill_crea_d() +
  scale_x_date(date_labels = "%b",
               date_breaks = '1 month')



