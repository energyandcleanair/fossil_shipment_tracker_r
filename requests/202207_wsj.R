# So I just have an idea which ports in Europe/US/Japan/South Korea used to import Russian oil and which ports in India, China, Egypt and etc now take some of that oil instead.


library(tidyverse)
library(ggplot2)
library(dplyr)

voyages <- read_csv('https://api.russiafossiltracker.com/v0/voyage?aggregate_by=destination_port,arrival_port_name,arrival_month&date_from=2022-01-01&date_to=2022-08-31&format=csv&status=completed&commodity_origin_iso2=RU&sort_by=value_tonne&commodity_group=oil,coal,gas')


top_countries <- voyages %>%
  # filter(status=='completed') %>%
  filter(value_eur>0) %>%
  group_by(destination_country) %>%
  summarise(value_tonne=sum(value_tonne)) %>%
  arrange(desc(value_tonne)) %>%
  head(12) %>%
  pull(destination_country)

top_ports <- voyages %>%
  filter(value_eur>0) %>%
  filter(destination_country %in% top_countries) %>%
  group_by(destination_country, arrival_port_name) %>%
  summarise(value_tonne=sum(value_tonne)) %>%
  ungroup() %>%
  arrange(desc(value_tonne)) %>%
  mutate(new_port_name = ifelse(row_number() < 50, arrival_port_name, 'Other')) %>%
  dplyr::select(destination_country, arrival_port_name, new_port_name) %>%
  distinct()


voyages %>%
  dplyr::select(country=destination_country, port_name=arrival_port_name, month=arrival_month, value_tonne) %>%
  arrange(country, month) %>%
  write_csv('voyages.csv', na='')



d <- top_ports %>%
  distinct(destination_country, new_port_name) %>%
  left_join(
    top_ports %>%
    left_join(
      voyages %>%
        group_by(destination_country, arrival_port_name, arrival_month) %>%
        summarise(value_tonne = sum(value_tonne, na.rm=T))) %>%
    dplyr::group_by(destination_country, new_port_name, arrival_month) %>%
    summarise(value_tonne=sum(value_tonne, na.rm=T))
  )

flourish <- d %>%
  dplyr::select(destination_country, new_port_name, arrival_month, value_tonne) %>%
  pivot_wider(id_cols=c(destination_country, arrival_month),
              names_from=new_port_name, values_from=value_tonne)

bind_cols(
  flourish %>% dplyr::select(-c(Other)),
  flourish %>% dplyr::select(Other)) %>%
write_csv('voyages_flourish.csv', na='')

ggplot(d) +
  geom_bar(aes(arrival_month, value_tonne, fill=new_port_name),
           stat='identity', show.legend=F) +
  facet_wrap(~destination_country)


