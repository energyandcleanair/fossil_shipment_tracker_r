library(tidyverse)
library(mapview)
#1. First we aim to follow a specific ship that leaves one of the East Sea/Black Sea ports and conducts a STS transfer, then unloads it's crude oil in Rotterdam or Amsterdam. We struggle to find the right ship as STS often occurs farther off the coast and our experience with sites like Marine Traffic is limited. Using your Alert tool, we find ships like the Ottoman Equity but have limited access to it's travel history and so can't determine if this ship is part of STS transfers - it doens't seem so.


v <- read_csv('https://api.russiafossiltracker.com/v0/voyage?commodity_destination_iso2=NL&commodity_origin_iso2=RU&date_from=2022-02-24&format=csv')

v %>%
  filter(is_sts) %>%
  as.list()

v %>%
  filter(is_sts) %>%
  sf::st_as_sf(coords=c('sts_position_lon','sts_position_lat')) %>%
  sf::st_set_crs(4326) %>%
  mapview::mapview()

mapview::mapview()


#2. Secondly we are trying to reliably quantify the increase of export of crude and oil products from Russia to Turkey and then from Turkey to the Netherlands, alligning with the decrease of direct oil traffic from Russia to the Netherlands, as this would clearly show how rerouting works


# From Russia and Turkey to Netherlands -----------------------------------
v <- read_csv('https://api.russiafossiltracker.com/v0/voyage?commodity_destination_iso2=NL&commodity_origin_iso2=RU,TR&arrival_date_from=2021-12-01&format=csv&aggregate_by=arrival_date,commodity,commodity_origin_iso2,status&commodity_grouping=split_gas_oil&date_to=-4')

v1 <- v %>%
  filter(!is.na(arrival_date),
         status=='completed',
         commodity %in% c('crude_oil', 'oil_products', 'oil_or_chemical')
  ) %>%
  group_by(date=arrival_date, commodity_group_name, commodity_origin_country) %>%
  summarise(value=sum(value_tonne, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(date=seq(min(.$date), max(.$date), by='day'), commodity_origin_country, commodity_group_name, fill=list(value=0))

# Export a formatted version
write_csv(v1 %>%
            spread(commodity_group_name, value) %>%
            filter(date >= '2022-01-01') %>%
            mutate(date=as.Date(date), unit='tonne'),
          'requests/202211_Nieuwsuur/shipments_to_nl.csv')


ggplot(v1 %>% rcrea::utils.running_average(14)) +
  geom_line(aes(date, value/1000, col=commodity_origin_country)) +
  facet_wrap(~commodity_group_name, scales='free_y') +
  rcrea::theme_crea() +
  scale_x_datetime(limits=c(as.POSIXct('2022-01-01'), NA)) +
  rcrea::scale_color_crea_d(palette='dramatic') +
  labs(title='Shipments from Turkey and Russia to the Netherlands',
       subtitle='Thousand tonnes per day, 14-day running average',
       x=NULL,
       y=NULL,
       caption='Source: CREA analysis.') +
  theme(legend.position = 'bottom') +
  rcrea::scale_y_crea_zero() +
  guides(color=guide_legend(nrow=1, title=NULL))

ggsave('requests/202211_Nieuwsuur/shipments_to_nl.jpg', width=8, height=6)


# Shipments from Russia to Turkey
v_tr <-read_csv('https://api.russiafossiltracker.com/v0/voyage?commodity_destination_iso2=TR&commodity_origin_iso2=RU,&arrival_date_from=2021-01-01&format=csv&aggregate_by=arrival_date,commodity,commodity_origin_iso2,status&commodity_grouping=split_gas_oil&date_to=-4')

v_tr1 <- v_tr %>%
  filter(!is.na(arrival_date),
         status=='completed',
         commodity %in% c('crude_oil', 'oil_products', 'oil_or_chemical')
  ) %>%
  group_by(date=arrival_date, commodity_group_name, commodity_origin_country) %>%
  summarise(value=sum(value_tonne, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(date=seq(min(.$date), max(.$date), by='day'), commodity_origin_country, commodity_group_name, fill=list(value=0))

# Export a formatted version
write_csv(v_tr1 %>%
            spread(commodity_group_name, value) %>%
            filter(date >= '2022-01-01') %>%
            mutate(date=as.Date(date),
                   unit='tonne'), 'requests/202211_Nieuwsuur/shipments_to_tr.csv')


ggplot(v_tr1 %>% rcrea::utils.running_average(14)) +
  geom_line(aes(date, value/1000, col=commodity_origin_country), show.legend = F) +
  facet_wrap(~commodity_group_name, scales='free_y') +
  rcrea::theme_crea() +
  scale_x_datetime(limits=c(as.POSIXct('2022-01-01'), NA)) +
  rcrea::scale_color_crea_d(palette='dramatic') +
  labs(title='Shipments from Russia to Turkey',
       subtitle='Thousand tonnes per day, 14-day running average',
       x=NULL,
       y=NULL,
       caption='Source: CREA analysis.') +
  theme(legend.position = 'bottom') +
  rcrea::scale_y_crea_zero() +
  guides(color=guide_legend(nrow=1, title=NULL))

ggsave('requests/202211_Nieuwsuur/shipments_to_tr.jpg', width=8, height=6)


# Check consistency with report
# NL ~ 80-90kt in Sept/Oct from Turkey
v %>%
  filter(commodity_origin_iso2=='TR',
         commodity %in% c('oil_products','oil_or_chemical'),
         arrival_date >= '2022-09-01',
         arrival_date <= '2022-10-31') %>%
  pull(value_tonne) %>%
  sum()
