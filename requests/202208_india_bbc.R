library(tidyverse)

commodity_colors <- list(Oil='#35416C',
                         Coal='#333333',
                         LNG='#f6b26b',
                         'Pipeline gas'='#741b47',
                         'Crude oil'='#35416C',
                         'Oil products'='#f6b26b')



# Arriving in India -------------------------------------------------------
voyages_to_india <- read_csv('https://api.russiafossiltracker.com/v0/voyage?destination_iso2=IN&date_from=2022-01-01&format=csv')


voyages_to_india %>%
  mutate(commodity_group=recode(commodity,
                                'lng'='LNG',
                                'natural_gas'='Pipeline gas',
                                .default=stringr::str_to_title(commodity_group))) %>%
  filter(departure_iso2=='RU',
         status=='completed',
         !is.na(commodity_group)) %>%
  group_by(date=lubridate::floor_date(arrival_date_utc,'month'),
           arrival_port_name,
           commodity_group) %>%
  summarise(ship_dwt=sum(ship_dwt)) %>%
  ggplot() +
  geom_bar(aes(date, ship_dwt, fill=commodity_group),
           stat='identity') +
  scale_fill_manual(values=commodity_colors)


# Leaving India -----------------------------------------------------------
voyages_from_india <- read_csv('https://api.russiafossiltracker.com/v0/voyage?departure_iso2=IN&date_from=2021-10-01&format=csv')

write.csv(voyages_from_india, 'results/shipments_from_sikka.csv', row.names = F)

top_recipients <- voyages_from_india %>%
  filter(commodity_destination_country!='India',
         status=='completed') %>%
  group_by(commodity_destination_country) %>%
  summarise(ship_dwt=sum(ship_dwt)) %>%
  arrange(desc(ship_dwt)) %>%
  pull(commodity_destination_country) %>%
  head(12)

voyages_from_india %>%
  # mutate(commodity_group=recode(commodity,
  #                               'lng'='LNG',
  #                               'natural_gas'='Pipeline gas',
  #                               .default=stringr::str_to_title(commodity_group))) %>%
  filter(status=='completed',
         grepl('Sik[k]?a', departure_port_name, ignore.case = T),
         commodity %in% c('crude_oil','oil_products','oil_or_chemicals'),
         commodity_destination_country %in% top_recipients) %>%
  mutate(commodity_destination_country=factor(commodity_destination_country, levels=top_recipients),
         commodity=factor(commodity,
                          levels=rev(c('crude_oil','oil_products')),
                          labels=rev(c('Crude oil','Oil products')))) %>%
  group_by(date=lubridate::floor_date(departure_date_utc,'month'),
           commodity,
           commodity_destination_country) %>%
  summarise(ship_dwt=sum(ship_dwt),
            n_shipments=n()) %>%
  ggplot(aes(date, ship_dwt/1000, fill=commodity)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=n_shipments), color='white', size=3, position=position_stack(vjust = .5)) +
  facet_wrap(~commodity_destination_country) +
  rcrea::theme_crea() +
  scale_fill_manual(values=commodity_colors[c('Crude oil','Oil products')],
                    name=NULL,
                    guide = guide_legend(reverse = TRUE)) +
  labs(title='Largest recipients of shipments from Sikka',
       subtitle='Tonnage and number of fossil fuel shipments by departure month',
       y='kilo tonne',
       caption='Source: CREA analysis.',
       x=NULL) +
  rcrea::scale_y_crea_zero() +
  scale_x_datetime(date_breaks = '1 month',
               date_labels = '%b %y')+
  theme(panel.grid.major.y = element_line(size=0.1),
        panel.border = element_rect(colour="white"),
        axis.line = element_line(size=0.1),
        legend.position="bottom",
        legend.direction="horizontal",
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggsave('results/shipments_from_sikka.jpg', width=8, height=6)
