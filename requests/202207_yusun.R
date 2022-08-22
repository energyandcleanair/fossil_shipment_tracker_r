library(tidyverse)
library(clipr)
library(ggplot2)
library(tidytext)
library(stringr)
library(rcrea)


# Collect data ------------------------------------------------------------


date_to <- '2022-07-31'
folder <- sprintf('results/yusun_til_%s', gsub( '-', '', date_to))

voyages <- read_csv('https://api.russiafossiltracker.com/v0/voyage?format=csv') %>%
  filter(commodity_origin_iso2 == 'RU') %>% # REMOVING KAZAK OIL!
  filter(arrival_date_utc >= '2022-02-24') %>%
  filter(arrival_date_utc <= date_to) %>%
  filter(status=='completed')

voyages_all <- read_csv('https://api.russiafossiltracker.com/v0/voyage?format=csv') %>%
  filter(commodity_origin_iso2 == 'RU') %>% # REMOVING KAZAK OIL!
  filter(arrival_date_utc >= '2021-12-01') %>%
  filter(arrival_date_utc <= date_to) %>%
  filter(status=='completed')

counter <- read_csv(sprintf('https://api.russiafossiltracker.com/v0/counter?format=csv&date_to=%s', date_to))

commodity_colors <- list(Oil='#35416C',
                         Coal='#333333',
                         LNG='#f6b26b',
                         'Pipeline gas'='#741b47')


dir.create(folder, showWarnings = F, recursive=T)
write_csv(voyages, file.path(folder, 'voyages.csv'))
write_csv(counter, file.path(folder, 'counter.csv'))

# voyages <- read_csv(file.path(folder, 'voyages.csv'))
# counter <- read_csv(file.path(folder, 'counter.csv'))

# For Taiwan, please exclude coal shipments without identified berth. I've done that for data I've provided to Taiwan because too much of the rest is iron ore, scrap etc
voyages <- voyages %>%
  filter(!(commodity=='coal' & destination_iso2 %in% c('TW') & is.na(arrival_berth_name)))


# Sanity check: now counter excludes TW coal without arrival berth as well, so the two should be equal
voyages %>%
  filter(commodity_destination_iso2 %in% c('TW','KR','JP')) %>%
  group_by(destination_iso2=commodity_destination_iso2, commodity_group) %>%
  summarise(value_mneur_voyage = sum(value_eur, na.rm=T)/1e6) %>%
  left_join(counter %>%
              filter(destination_iso2 %in% c('TW','KR','JP')) %>%
              group_by(destination_iso2, commodity_group) %>%
              summarise(value_mneur_counter = sum(value_eur, na.rm=T)/1e6))




# Largest countries -------------------------------------------------------


#-Largest importers by commodity, latest update (CSV file - please see attached image)
data_1 <- bind_rows(
              # We use voyages for shipments to remove coal without arrival in Taiwan
            voyages %>%
              filter(commodity %in% c('coal', 'oil_products', 'crude_oil')),

            # And counter for the rest
            counter %>%
              filter(!commodity %in% c('coal', 'oil_products', 'crude_oil')) %>%
              rename(commodity_destination_iso2=destination_iso2,
                     commodity_destination_country=destination_country)
            ) %>%
  mutate(commodity_group=recode(commodity,
                                'lng'='LNG',
                                'natural_gas'='Pipeline gas',
                                .default=commodity_group)) %>%
  mutate(commodity_group=recode(commodity_group,
                                oil='Oil',
                                coal='Coal')) %>%
  filter(commodity_group %in% c('Pipeline gas', 'Oil', 'Coal', 'LNG')) %>%
  group_by(commodity_destination_iso2, commodity_destination_country, commodity_group) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
  arrange(commodity_group, desc(value_eur))

write.csv(data_1, file.path(folder, 'largest_importing_countries.csv'))

data_1 %>%
    group_by(commodity_group) %>%
    top_n(10, value_eur) %>%
    mutate(country=reorder_within(commodity_destination_country, value_eur, commodity_group)) %>%
    ggplot(aes(value_eur/1e6, country, fill=commodity_group)) +
           geom_bar(stat = 'identity', show.legend = F) +
           scale_y_reordered() +
           facet_wrap(~ commodity_group, scales="free") +
  scale_x_continuous(expand = expansion(mult=c(0,0.1))) +
  scale_fill_manual(values=commodity_colors) +
  rcrea::theme_crea() +
  labs(y=NULL, x='mnEUR',
       title='Largest importers of fossil fuels from Russia',
       subtitle=sprintf('From %s to %s',
                        strftime(min(counter$date), '%d %B %Y'),
                        strftime(max(counter$date), '%d %B %Y')
                        ))

ggsave(file.path(folder, 'largest_importing_countries.jpg'), width=10, height=8)


data_1_summary <- data_1 %>%
  filter(commodity_destination_iso2 %in% c('TW','JP','KR')) %>%
  group_by(commodity_destination_country, commodity_group) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
  bind_rows(data_1 %>%
              filter(commodity_destination_iso2 %in% c('TW','JP','KR')) %>%
              group_by(commodity_destination_country) %>%
              summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
              mutate(commodity_group='Total')) %>%
  bind_rows(data_1 %>%
              filter(commodity_destination_iso2 %in% c('TW','JP','KR')) %>%
              group_by() %>%
              summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
              mutate(commodity_group='Total', commodity_destination_country='Total')) %>%
  arrange(commodity_destination_country, commodity_group) %>%
  mutate(value_mneur = round(value_eur / 1e6, 2),
         value_mnusd = round(value_usd / 1e6, 2)) %>%
  select(-c(value_eur, value_usd))



# Timeseries --------------------------------------------------------------



# Time series of JP, TW, KR
# voyages_all %>%
#   filter(commodity_destination_iso2 %in% c("KR","JP","TW")) %>%
#   filter(!is.na(commodity_group)) %>%
#   group_by(commodity_destination_country, date=as.Date(arrival_date_utc), commodity_group) %>%
#   summarise_at(c('value_tonne'), sum, na.rm=T) %>%
#   rcrea::utils.running_average(30, vars_to_avg = c('value_tonne')) %>%
#   ggplot(aes(date, value_tonne, col=commodity_group)) +
#   geom_line()+
#   facet_wrap(~commodity_destination_country)


# Largest ports -----------------------------------------------------------

#-Largest importing oil/coal/LNG ports for SK/JP/TW, latest update (CSV file, API file and graphs)
data_2 <- voyages %>%
  # filter(destination_iso2 %in% c("KR","JP","TW")) %>%
  mutate(commodity_group=recode(commodity_group,
                                oil='Oil',
                                gas='LNG',
                                coal='Coal')) %>%
  mutate(arrival_port_name=ifelse(grepl('yeosu.*', arrival_port_name, T), "Yeosu", arrival_port_name)) %>%
  group_by(destination_country, destination_iso2, arrival_port_name, commodity_group) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
  filter(!is.na(commodity_group)) %>%
  arrange(commodity_group, desc(value_eur))

write.csv(data_2, file.path(folder, 'largest_importing_ports.csv'))

data_2 %>%
  group_by(commodity_group) %>%
  top_n(10, value_eur) %>%
  mutate(port=reorder_within(sprintf('%s (%s)', stringr::str_to_title(arrival_port_name), destination_iso2), value_eur, commodity_group)) %>%
  ggplot(aes(value_eur / 1e6, port, fill=commodity_group)) +
  geom_bar(stat = 'identity', show.legend = F) +
  scale_y_reordered() +
  scale_fill_manual(values=commodity_colors) +
  facet_wrap(~ commodity_group, scales="free") +
  scale_x_continuous(expand = expansion(mult=c(0,0.1))) +
  rcrea::theme_crea() +
  labs(y=NULL, x='mnEUR')

ggsave(file.path(folder, 'largest_importing_ports.jpg'), width=10, height=6)


#-Top importing companies counter for SK/JP/TW (including the monetary valuation of imports from the beginning of invasion to the latest updated date, and which fossil fuels, CSV and API files)
data_3 <- voyages %>%
  filter(commodity_destination_iso2 %in% c("KR","JP","TW")) %>%
  mutate(commodity_group=recode(commodity_group,
                                oil='Oil',
                                gas='LNG',
                                coal='Coal')) %>%
  mutate(arrival_berth_owner=tidyr::replace_na(arrival_berth_owner, "Unknown")) %>%
  mutate(arrival_berth_owner=ifelse(grepl('Authority|Ports Corporation', arrival_berth_owner), "Unknown", arrival_berth_owner)) %>%
  group_by(commodity_destination_iso2, arrival_berth_owner, commodity_group) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
  filter(!is.na(commodity_group)) %>%
  arrange(commodity_group, desc(value_eur))



write.csv(data_3, file.path(folder, 'largest_importing_companies.csv'))

data_3 %>%
  group_by(commodity_group) %>%
  top_n(10, value_eur) %>%
  mutate(company=reorder_within(str_wrap(sprintf('%s (%s)', arrival_berth_owner, commodity_destination_iso2), 20), value_eur, commodity_group)) %>%
  ggplot(aes(value_eur/1e6, company, fill=commodity_group, alpha=grepl('Unknown', arrival_berth_owner))) +
  geom_bar(stat = 'identity', show.legend = F) +
  scale_y_reordered() +
  facet_wrap(~ commodity_group, scales="free") +
  scale_x_continuous(expand = expansion(mult=c(0,0.1))) +
  scale_fill_manual(values=commodity_colors) +
  scale_alpha_manual(values=c(1, 0.5)) +
  rcrea::theme_crea() +
  labs(y=NULL, x='mnEUR',
       title='Largest importing companies of fossil fuels from Russia',
       subtitle=sprintf('Japan, South Korea and Taiwan only. From %s to %s',
                        strftime(min(counter$date), '%d %B %Y'),
                        strftime(max(counter$date), '%d %B %Y')
       )) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour='grey90'),
        )

ggsave(file.path(folder, 'largest_importing_companies.jpg'), width=10, height=6)


#-TIMESERIES OF Top importing companies counter for SK/JP/TW (including the monetary valuation of imports from the beginning of invasion to the latest updated date, and which fossil fuels, CSV and API files)

data_3_ts <- voyages %>%
  filter(commodity_destination_iso2 %in% c("KR","JP","TW")) %>%
  mutate(commodity_group=recode(commodity_group,
                                oil='Oil',
                                gas='LNG',
                                coal='Coal')) %>%
  group_by(commodity_destination_iso2, arrival_berth_owner, commodity_group, date=lubridate::floor_date(arrival_date_utc)) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T) %>%
  filter(!is.na(commodity_group)) %>%
  arrange(commodity_group, desc(value_eur), date) %>%
  mutate(arrival_berth_owner=tidyr::replace_na(arrival_berth_owner, "Unknown"))

write.csv(data_3_ts, file.path(folder, 'largest_importing_companies_timeseries.csv'))

top_companies <- data_3 %>% group_by(commodity_group) %>% top_n(10, arrival_berth_owner)

# data_3_ts %>%
#   filter(arrival_berth_owner %in% top_companies$arrival_berth_owner) %>%
#   filter(!grepl('Unknown.*', arrival_berth_owner)) %>%
#   mutate(date=as.Date(date)) %>%
#   # tidyr::complete(date=seq.Date(min(.$date), max(.$date), by='day')) %>%
#   group_by(commodity_group) %>%
#   # rcrea::utils.running_average(7, vars_to_avg = c("value_eur", "value_usd")) %>%
#   mutate(company=reorder_within(str_wrap(sprintf('%s (%s)', arrival_berth_owner, destination_iso2), 20), value_eur, commodity_group)) %>%
#   ggplot(aes(date, value_eur/1e6, color=company)) +
#   geom_line() +
#   facet_wrap(~ commodity_group, scales="free") +
#   scale_y_continuous(expand = expansion(mult=c(0,0.1))) +
#   scale_fill_manual(values=commodity_colors) +
#   rcrea::theme_crea() +
#   labs(y=NULL, x='mnEUR',
#        title='Largest importing companies of fossil fuels from Russia',
#        subtitle=sprintf('From %s to %s',
#                         strftime(min(counter$date), '%d %B %Y'),
#                         strftime(max(counter$date), '%d %B %Y')
#        ))
#
# ggsave(file.path(folder, 'largest_importing_companies_timeseries.jpg'), width=10, height=8)


# Hi Hubert, just want to confirm that from the July 25 company data, for JERA's total imports figure, it's accurate to add all four mentions of JERA together (JERA and Nippon Steel, JERA and Tokyo Gas, JERA Higashi Ogashima etc)? This comes out to ~536.3 million euros.
jera_voyages <- voyages %>%e
  filter(grepl('JERA', arrival_berth_owner, ignore.case = T)) %>%
  group_by(commodity, arrival_berth_owner) %>%
  summarise_at(grep('value_', names(.), value=T), sum, na.rm=T) %>%
  ungroup() %>%
  do(bind_rows(., data.frame(.) %>%
                 group_by() %>%
                 summarise_if(is.numeric, sum, na.rm=T) %>%
                 mutate(arrival_berth_owner='Total', commodity='Total')))

write.csv(jera_voyages, file.path(folder, 'jera_companies.csv'))

#-How many euros worth of Russian crude oil transported from South Korea to Chinese tankers/China, latest calculation
data_4 <- voyages %>%
  filter(destination_iso2=='KR',
         commodity_destination_iso2=='CN') %>%
  group_by(commodity) %>%
  summarise_at(c('value_eur','value_usd'), sum, na.rm=T)

write.csv(data_4, file.path(folder, 'south_korea_to_china.csv'))



#-Counter for SK/JP/TW fossil fuel imports (monetary valuation) and how much of that goes to Russian federal budget, latest update

# From Andrei
commodity_revenue_usd_tonne <- list(
  coal=7.82,
  gas=174.77,
  oil=174.77
)

data_5 <- counter %>%
  filter(destination_iso2 %in% c('KR','JP','TW')) %>%
  group_by(destination_country, destination_iso2, commodity_group) %>%
  summarise_at(c('value_tonne', 'value_usd', 'value_eur'), sum, na.rm=T) %>%
  filter(!is.na(commodity_group)) %>%
  mutate(budget_revenue_usd=value_tonne * recode(commodity_group, !!!commodity_revenue_usd_tonne))

write.csv(data_5, file.path(folder, 'budget_revenue.csv'))


data_5 %>%
  mutate(commodity_group=recode(commodity_group,
                                oil='Oil',
                                gas='LNG',
                                coal='Coal')) %>%
  ggplot(aes(destination_country, budget_revenue_usd/1e6, fill=commodity_group)) +
  geom_bar(stat = 'identity', show.legend = T) +
  scale_y_continuous(expand = expansion(mult=c(0,0.1))) +
  scale_fill_manual(values=commodity_colors[c('Oil','LNG','Coal')], name=NULL, guide = guide_legend(reverse = TRUE)) +
  rcrea::theme_crea() +
  labs(x=NULL, y='mnUSD',
       title='Russia budget revenue from fossil fuel purchase',
       subtitle=sprintf('From %s to %s',
                        strftime(min(counter$date), '%d %B %Y'),
                        strftime(max(counter$date), '%d %B %Y')
       ))

ggsave(file.path(folder, 'budget_revenue.jpg'), width=10, height=6)



key_numbers <- paste(
  sprintf('Total Russian budget revenue from TW, KR, JP: %.2f mnUSD', sum(data_5$budget_revenue_usd)/1e6),
  sprintf('Total spent from TW, KR, JP: %.2f mnEUR', data_1_summary[data_1_summary$commodity_destination_country=='Total' & data_1_summary$commodity_group=='Total', 'value_mneur']),
  sprintf('Total spent from TW, KR, JP: %.2f mnUSD', data_1_summary[data_1_summary$commodity_destination_country=='Total' & data_1_summary$commodity_group=='Total', 'value_mnusd']),
  sprintf('Total of Russian crude oil from KR to CN: %.2f mnEUR', data_4[data_4$commodity=='crude_oil', 'value_eur']/1e6),
  sprintf('Total for JERA: %.2f mnEUR (%s)', jera_voyages[jera_voyages$arrival_berth_owner=='Total', 'value_eur']/1e6, paste(jera_voyages$arrival_berth_owner, collapse="; ")),

  capture.output(format.data.frame(data_1_summary %>% ungroup()))  %>% paste(collapse="\n", sep=""),
  sep="\n\n"
)
write(key_numbers, file.path(folder, 'key_numbers.txt'))



# Old exploration ---------------------------------------------------------
#
# voyages %>%
#   filter(strftime(floor_date(arrival_date_utc, 'month') , '%Y-%m-%d')== '2022-06-01',
#          commodity_origin_iso2 == 'RU') %>%
#   group_by(arrival_port_name, destination_country, commodity) %>%
#   summarise(value_mn_eur=sum(value_eur, na.rm=T) / 1e6) %>%
#   arrange(desc(value_mn_eur))%>%
#   filter(grepl('yeosu', arrival_port_name, T))
#
#
# voyages %>%
#   filter(
#     # strftime(floor_date(arrival_date_utc, 'month') , '%Y-%m-%d')== '2022-06-01',
#          commodity_origin_iso2 == 'RU') %>%
#   group_by(arrival_port_name, destination_country, commodity_destination_country, arrival_berth_name, commodity) %>%
#   summarise(value_mn_eur=sum(value_eur, na.rm=T) / 1e6) %>%
#   arrange(desc(value_mn_eur)) %>%
#   filter(destination_country == 'South Korea') %>%
#   View()
#   # filter(grepl('yeosu', arrival_port_name, T))
#
#
# # Largest crude-oil importing ports
# voyages %>%
#   filter(
#     arrival_date_utc >= '2022-02-24',
#     arrival_date_utc <= '2022-06-03',
#     commodity_origin_iso2 == 'RU',
#     commodity %in% c('oil_products', 'lng', 'crude_oil', 'coal')
#     ) %>%
#   group_by(commodity, destination_country, arrival_port_name) %>%
#   summarise(value_mn_eur=round(sum(value_eur, na.rm=T) / 1e6),
#             kt=round(sum(value_tonne)/1000),
#             count=n(),
#             companies = paste(unique(setdiff(arrival_berth_owner, NA)), collapse="; "),
#             latest_date = lubridate::floor_date(max(arrival_date_utc), 'day')
#             ) %>%
#   arrange(desc(value_mn_eur)) %>%
#   group_by(commodity) %>%
#   top_n(10, value_mn_eur) %>%
#   arrange(commodity)
#
#
# clipr::write_last_clip()
#
# voyages %>%
#   filter(
#     arrival_date_utc >= '2022-02-24',
#     arrival_date_utc <= '2022-06-03',
#     # commodity_origin_iso2 == 'RU',
#     departure_iso2 == 'RU',
#     destination_iso2 %in% c('JP', 'KR', 'TW'),
#     commodity %in% c('oil_products', 'lng', 'crude_oil', 'coal')
#   ) %>%
#   group_by(arrival_berth_owner) %>%
#   summarise(value_mn_eur=round(sum(value_eur, na.rm=T) / 1e6)) %>%
#   arrange(desc(value_mn_eur))
#
# clipr::write_last_clip()
