# Jan and Hubert, I promised Shadia we would share two things:
library(tidyverse)
library(lubridate)

#   -the estimates of how gas flaring has developed
flaring <- read_csv('https://api.russiafossiltracker.com/v0/flaring?aggregate_by=date,facility_type&date_from=2018-01-01&format=csv')
flaring %>%
  distinct(buffer_km, type)

flaring %>%
  # tidyr::complete(date, type, fill=list(value=0)) %>%
  rcrea::utils.running_average(14, min_values = 10) %>%
  mutate(date000=`year<-`(date,2000),
         year=factor(lubridate::year(date)))  %>%
  ggplot() +
  geom_line(aes(date000, value, col=year, size=year)) +
  facet_wrap(~type, scales='free_y', ncol = 1) +
  rcrea::theme_crea() +
  scale_color_brewer(palette='Reds', name=NULL) +
  scale_size_manual(values=c(`2018`=1, `2019`=1, `2020`=1, `2021`=1, `2022`=2) / 3, name=NULL) +
  labs(title='Gas flaring intensity along Russian gas infrastructure',
       caption='Source: CREA analysis using VIIRS Nightfire (VNF) nightly data produced by the Earth Observation Group, Payne Institute for Public Policy, Colorado School of Mines.',
       y='Flaring intensity, 14-day running average',
       x=NULL
       ) +
    guides(color= guide_legend(), size=guide_legend())

ggsave('requests/202210_reuters/flaring_ts.jpg', width=12, height=10)

# -data on how the breakdown of Europe's gas imports has changed by country of origin (with LNG as one "country")

flows_raw <- read_csv('https://api.russiafossiltracker.com/v0/entsogflow?date_from=2020-12-16&format=csv&aggregate_by=commodity_destination_iso2,commodity_origin_iso2,date')


regions <-
  list(
    Norway='Norway',
    Algeria='Algeria',
    Russia='Russia',
    Ukraine='Russia',
    LNG='LNG',
    `United Kingdom`='United Kingdom',
    Azerbaijan='Azerbaijan',
    Others='Others'
  )


force_as_europe <- c('Switzerland', 'Albania', 'Serbia')
force_as_russia <- c('Ukraine')

flows_raw <- flows_raw %>%
  mutate(
    commodity_destination_region=case_when(commodity_destination_country %in% force_as_europe ~ 'EU', T~commodity_destination_region),
    commodity_origin_region=case_when(commodity_origin_country %in% force_as_europe ~ 'EU', T~commodity_origin_region),
    commodity_origin_region=case_when(commodity_origin_country %in% force_as_russia ~ 'Russia', T~commodity_origin_region),
    commodity_origin_country=case_when(commodity_origin_country %in% force_as_russia ~ 'Russia', T~commodity_origin_country),
    commodity_destination_country=case_when(commodity_destination_country %in% force_as_russia ~ 'Russia', T~commodity_destination_country),
    )

# TO Europe
flows_import <- flows_raw %>%
  filter(commodity_destination_region=='EU',
         commodity_origin_region!='EU') %>%
  select(from=commodity_origin_country, to=commodity_destination_region, date, value_m3) %>%
  group_by(from, to, date) %>%
  summarise(value_m3_import=sum(value_m3, na.rm=T))

# FROM Europe
flows_export <- flows_raw %>%
  filter(commodity_destination_region!='EU',
         commodity_origin_region=='EU') %>%
  select(from=commodity_destination_country, to=commodity_origin_region, date, value_m3) %>%
  group_by(from, to, date) %>%
  summarise(value_m3_export=sum(value_m3, na.rm=T))

flows_net <- flows_import %>%
  full_join(flows_export) %>%
  mutate(value_m3=tidyr::replace_na(value_m3_import,0)-tidyr::replace_na(value_m3_export,0)) %>%
  group_by(date, from) %>%
  summarise(value_m3=sum(value_m3))

flows_net %>%
  group_by(from) %>%
  summarise(value_m3=sum(value_m3)) %>%
  arrange(desc(value_m3))

flows_net %>%
  mutate(from_region=recode(from, !!!regions, .default='Others')) %>%
  mutate(from_region=factor(from_region, rev(unique(unlist(regions))))) %>%
  group_by(date, from_region) %>%
  summarise(value=max(sum(value_m3, na.rm=T),0)) %>%
  ungroup() %>%
  tidyr::complete(from_region, date, fill=list(value=0)) %>%
  rcrea::utils.running_average(14, min_values = 14) %>%
  group_by(date) %>%
  mutate(share=value/sum(value, na.rm=T)) %>%
  ungroup() %>%
  ggplot() +
  geom_area(aes(date, share, fill=from_region)) +
  rcrea::theme_crea() +
  scale_y_continuous(labels=scales::percent, limits=c(0,1.001), expand = expansion(mult=0)) +
  # scale_fill_discrete(name=NULL) +
  rcrea::scale_fill_crea_d(name=NULL) +
  scale_x_date(limits=c(as.Date('2021-01-01'), NA),
               expand=expansion(mult=0)) +
  labs(title='Share of Europe natural gas imports by origin',
       caption='Source: CREA based on ENTSOG. Europe is hereby defined as EU-27 plus Switzerland, Serbia, and Albania.',
       x=NULL,
       y=NULL)

ggsave('requests/202210_reuters/europe_imports_share.jpg', width=12, height=6)

flows_net %>%
  mutate(from_region=recode(from, !!!regions, .default='Others')) %>%
  mutate(from_region=factor(from_region, rev(unique(unlist(regions))))) %>%
  group_by(date, from_region) %>%
  summarise(value=max(sum(value_m3, na.rm=T),0)) %>%
  ungroup() %>%
  tidyr::complete(from_region, date, fill=list(value=0)) %>%
  rcrea::utils.running_average(14, min_values = 14) %>%
  ggplot() +
  geom_area(aes(date, value/1e6, fill=from_region)) +
  rcrea::theme_crea() +
  scale_y_continuous(limits=c(0,NA), expand = expansion(mult=0)) +
  rcrea::scale_fill_crea_d(name=NULL) +
  scale_x_date(limits=c(as.Date('2021-01-01'), NA),
               expand=expansion(mult=0)) +
  labs(title='Europe natural gas imports by origin',
       caption='Source: CREA based on ENTSOG. Europe is hereby defined as EU-27 plus Switzerland, Serbia, and Albania.',
       x=NULL,
       y='million cubic meter per day (14-day running average)')

ggsave('requests/202210_reuters/europe_imports.jpg', width=12, height=6)


write_csv(flows_net %>% filter(date>='2021-01-01'), 'requests/202210_reuters/europe_imports.csv')

flows_net %>%
  mutate(from=recode(from, !!!regions, .default='Others')) %>%
  ungroup() %>%
  filter(date > max(date) - lubridate::days(14)) %>%
  group_by(from) %>%
  summarise(value_m3=sum(value_m3), count=n()) %>%
  mutate(share=scales::percent(value_m3/sum(value_m3))) %>%
  arrange(desc(value_m3)) %>%
  select(from, share)
