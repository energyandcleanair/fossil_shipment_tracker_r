# Comparing numbers sent to Simon vs our db
library(rcrea)
library(tidyverse)

bpd_20220620_20220719 = list(
  'Netherlands' = 514308,
  'Italy' = 348185,
  'Romania' = 140083,
  'Bulgaria' = 115325,
  'Poland' = 77049,
  'Spain' = 58916,
  'Finland' = 47994,
  'Greece' = 42100,
  'Croatia' = 32770,
  'Belgium' = 11323
)


bpd_20220719_202208_17 = list(
  'Italy' = 462665,
  'Netherlands' = 272586,
  'Romania' = 115731,
  'Bulgaria' = 115003,
  'Poland' = 46935,
  'Greece' = 26291,
  'Finland' = 24088
)

barrel_per_tonne <- 7.49

d_simon <- bind_rows(
  tibble(
    country=names(bpd_20220620_20220719),
    value_bpd=unlist(bpd_20220620_20220719),
    period = '2022-06-20 to\n 2022-07-19',
    source='TankerTrackers'
  ),
  tibble(
    country=names(bpd_20220719_202208_17),
    value_bpd=unlist(bpd_20220719_202208_17),
    period = '2022-07-19 to\n 2022-08-17',
    source='TankerTrackers'
  ))


periods <- tibble(
  date_from=c('2022-06-20', '2022-07-19'),
  date_to=c('2022-07-19', '2022-08-17')) %>%
  mutate(period = sprintf('%s to\n %s', date_from, date_to),
         ndays = as.integer(as.Date(date_to) - as.Date(date_from)) + 1)


voyages <- read_csv('https://api.russiafossiltracker.com/v0/voyage?commodity=crude_oil&destination_region=EU28&format=csv')

d_crea <- voyages %>%
  filter(commodity_origin_iso2=='RU') %>%
  tidyr::crossing(periods) %>%
  filter(arrival_date_utc >= date_from,
         arrival_date_utc <= date_to,
         ) %>%
  group_by(country=commodity_destination_country,
           period,
           ndays) %>%
  summarise(value_tonne=sum(value_tonne)) %>%
  mutate(value_bpd=value_tonne/ndays*barrel_per_tonne) %>%
  arrange(desc(value_bpd)) %>%
  mutate(source='CREA') %>%
  select(country, period, value_bpd, source)


bind_rows(d_crea, d_simon) %>%
  ungroup() %>%
  tidyr::complete(country, period, source, fill=list(value_bpd=0)) %>%
  ggplot(aes(period, value_bpd/1000, fill=source)) +
  geom_bar(stat='identity', position='dodge') +
  facet_wrap(~reorder(country, -value_bpd)) +
  rcrea::scale_y_crea_zero() +
  rcrea::theme_crea() +
  scale_fill_crea_d(name=NULL) +
  labs(title='EU Crude oil imports - TankerTrackers vs CREA',
       subtitle='Over two periods of 30 days.',
       y='thousand barrels per day',
       x=NULL,
       caption='Source: CREA analysis.')


bind_rows(d_crea, d_simon) %>%
  group_by(period, source) %>%
  summarise(value_bpd=sum(value_bpd)) %>%
  ggplot(aes(period, value_bpd/1000, fill=source)) +
  geom_bar(stat='identity', position='dodge') +
  # facet_wrap(~country) +
  rcrea::scale_y_crea_zero() +
  rcrea::theme_crea() +
  scale_fill_crea_d(name=NULL) +
  labs(title='EU Crude oil imports - TankerTrackers vs CREA',
       subtitle='Over two periods of 30 days.',
       y='thousand barrels per day',
       x=NULL,
       caption='Source: CREA analysis.')
