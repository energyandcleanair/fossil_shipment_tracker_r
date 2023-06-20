# Insurers
library(tidyverse)

insurers <- read_csv('https://api.russiafossiltracker.com/v0/voyage?date_from=2022-02-24&date_to=2022-08-24&aggregate_by=ship_insurer_country,commodity,commodity_destination_iso2&commodity_origin_iso2=RU&format=csv&commodity_grouping=split_gas') %>%
  filter(!is.na(commodity_group),
         commodity_destination_iso2 != 'RU') %>%
  mutate(ship_insurer_country=tidyr::replace_na(ship_insurer_country, "Unknown"))


insurers_share <- insurers %>%
  group_by(ship_insurer_country, commodity_group) %>%
  summarise(value_tonne=sum(value_tonne)) %>%
  bind_rows(
    insurers %>%
      group_by(ship_insurer_country) %>%
      summarise(value_tonne=sum(value_tonne)) %>%
      mutate(commodity_group='total')
  ) %>%
  group_by(commodity_group) %>%
  mutate(share=value_tonne / sum(value_tonne)) %>%
  ungroup()


top_insurers <- insurers_share %>%
  filter(commodity_group=='total') %>%
  arrange(share) %>%
  pull(ship_insurer_country)


insurers_share <- insurers_share %>%
  mutate(ship_insurer_country=factor(ship_insurer_country, levels=top_insurers)) %>%
  mutate(share_txt=ifelse(share > 0.02, scales::percent(share, 1), '')) %>%
  ungroup()


ggplot(insurers_share, aes(stringr::str_to_title(commodity_group), share, fill=ship_insurer_country)) +
  geom_bar(stat='identity') +
  geom_text(aes(label=share_txt),
            position=position_stack(vjust=0.5),
            col='white') +
  rcrea::theme_crea() +
  scale_fill_manual(values=rev(rcrea::makepal('CREA', 1, T)(length(top_insurers))),  name=NULL) +
  scale_y_continuous(labels=scales::percent_format(1),
                     expand = expansion(mult=0)) +
  labs(title='Incorporation country of ship insurers',
       subtitle='Share of Russian fossil fuel export tonnage during 24 February - 24 August 2-22',
       caption='Source: CREA analysis.',
       x=NULL,
       y='Share of tonnage')

ggsave('reports/202209_sixmonth/ship_insurers.jpg', width=10, height=6)

# Owners ------------------------------------------------------------------

owner_ts <- read_csv('https://api.russiafossiltracker.com/v0/voyage?date_from=2021-12-01&format=csv') %>%
  filter(!is.na(commodity_group),
         commodity_origin_iso2=='RU',
         commodity_destination_iso2!='RU',
         status=='completed') %>%
  group_by(ship_owner_country, date=lubridate::date(arrival_date_utc)) %>%
  summarise(value=sum(value_tonne))

owner_ts <- owner_ts %>%
  ungroup() %>%
  tidyr::complete(ship_owner_country, date, fill=list(value=0)) %>%
  rcrea::utils.running_average(30) %>%
  mutate(ship_owner_country=tidyr::replace_na(ship_owner_country, 'Unknown'))


top_owners <- owner_ts %>%
  group_by(ship_owner_country) %>%
  summarise(value=sum(value, na.rm=T)) %>%
  arrange(desc(value)) %>%
  head(20) %>%
  pull(ship_owner_country)

ggplot(owner_ts %>%
         # filter(commodity=='crude_oil') %>%
         filter(ship_owner_country %in% top_owners) %>%
         mutate(ship_owner_country=factor(ship_owner_country, top_owners))) +
  geom_line(aes(date, value / 1000, col=ship_owner_country),
            show.legend = F) +
  rcrea::theme_crea() +
  rcrea::scale_y_crea_zero() +
  facet_wrap(~ship_owner_country) +
  labs(y='thousand tonne per day, 30-day running average',
       x=NULL,
       title='Incorporation country of ship owners',
       subtitle='Shipment of Russian fossil fuel in thousand tonne per day | Top 20 countries',
       caption='Source: CREA analysis. Shipment from Russia to other countries only.')

ggsave('reports/202209_sixmonth/ship_owners_ts.jpg', width=10, height=6)




# Extracting missing companies --------------------------------------------

v <- read_csv('https://api.russiafossiltracker.com/v0/voyage?date_from=2021-12-01&format=csv')

v %>%
  filter(is.na(ship_owner_country),
         !is.na(commodity_group),
         !is.na(ship_owner)) %>%
  group_by(ship_owner, ship_owner_imo) %>%
  summarise(value=sum(value_tonne)) %>%
  arrange(desc(value)) %>%
  select(-c(value)) %>%
  write_csv('missing_owners.csv')

