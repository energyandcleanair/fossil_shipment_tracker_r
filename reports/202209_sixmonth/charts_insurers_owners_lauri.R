library(tidyverse)
# library(magrittr)
library(lubridate)
library(pbapply)
library(countrycode)
library(creahelpers)
library(rcrea)

output_dir <- 'reports/202209_sixmonth/'

  #country shares in ship ownership
  read_csv('https://api.russiafossiltracker.com/v0/voyage?date_from=2022-01-01&format=csv',
           guess_max=10000) %>%
    filter(!is.na(commodity_group),
           commodity_origin_iso2=='RU') ->
    voy

  #tanker capacity used plot
  voy %>%
    mutate(arrival_date=lubridate::date(arrival_date_utc), departure_date=lubridate::date(departure_date_utc)) %>%
    # recode_commodity() %>%
    filter(commodity %in% c('oil_products', 'lng', 'crude_oil', 'coal')) %>%
    filter(commodity != 'Others',
           status %in% c('completed', 'ongoing')) %>%
    replace_na(list(arrival_date=Sys.Date()+7)) ->
    voy.in

  seq.Date(ymd('2022-01-01'), Sys.Date(), by='d') %>%
    pblapply(function(date) {
      voy.in %>% filter(departure_date<=!!date, arrival_date>=!!date) %>%
        group_by(commodity, destination_country, destination_region,
                 ship_owner_country, ship_owner_iso2, ship_owner_region,
                 ship_insurer_country, ship_insurer_iso2, ship_insurer_region) %>%
        summarise(across(value_tonne, sum)) %>%
        mutate(date=!!date)
    }) %>% bind_rows() -> capacity_use


  capacity_use %>%
    group_by(ship_owner_country, ship_owner_iso2) %>%
    filter(date>='2022-06-01') %>%
    summarise(across(value_tonne, sum)) %>%
    arrange(desc(value_tonne)) ->
    ship_owner_ranking

  # capacity_use %<>% mutate(iso2=ship_owner_country %>% countrycode('country.name.en', 'iso2c'))

  capacity_use %>%
    # mutate(EU=iso2 %in% codelist$iso2c[!is.na(codelist$eu28)]) %>%
    mutate(EU=ship_owner_region=='EU') %>%
    filter(month(date) == 7) %>%
    mutate(ship_owner_country =
             case_when(ship_owner_country %in% ship_owner_ranking$ship_owner_country[1:10] ~ ship_owner_country,
                       EU~'Other EU', T~'Others') %>% na.cover('Unknown')) %>%
    group_by(commodity, ship_owner_country, date) %>% summarise(across(value_tonne, sum)) %>%
    group_by(commodity, ship_owner_country) %>% summarise(across(value_tonne, mean)) %>%
    mutate(ship_owner_country =
             factor(ship_owner_country,
                    levels=rev(c('Other EU', ship_owner_ranking$ship_owner_country[1:10], 'Others')))) %>%
    write_csv(paste0(output_dir,'Ship capacity carrying Russian fossil fuels in July, by owner.csv')) %>%
    group_by(commodity) %>%
    mutate(share = value_tonne / sum(value_tonne)) ->
    capacity_plot

  #
  # capacity_plot <- capacity_plot %>%
  #   group_by(ship_owner_country) %>%
  #   summarise(across(value_tonne, sum)) %>%
  #   mutate(share = value_tonne / sum(value_tonne))

  capacity_plot %>%
    ggplot(aes(commodity, value_tonne/1e6, fill=ship_owner_country)) + geom_col() +
    geom_text(aes(label=scales::percent(share, accuracy=1),
                  alpha=ifelse(share<.05 & commodity != 'Crude oil' | commodity=='LNG', 0, 1)),
              position = position_stack(vjust = .5),
              col='white') +
    theme_crea() +
    # scale_fill_crea_d() +
    labs(title='EU-owned ships carry most Russian fossil fuel shipments',
         subtitle='Average ship capacity loaded with Russian fossil fuels in July, by country of ship owner',
         y='deadweight tonnage, million',
         fill='Ship owner country') +
    scale_y_continuous(expand=expansion(mult=c(0,.05))) +
    scale_alpha(guide='none', range = c(0,1))

ggsave(paste0(output_dir,'Ship capacity carrying Russian fossil fuels, by owner.png'),
       width=8,height=6)








###insurer

capacity_use %>% group_by(ship_insurer_country) %>%
  filter(date>='2022-06-01') %>%
  summarise(across(value_tonne, sum)) %>% arrange(desc(value_tonne)) ->
  ship_insurer_ranking

capacity_use %<>% mutate(iso2=ship_insurer_country %>% countrycode('country.name.en', 'iso2c'))


capacity_use %>%
  mutate(EU=iso2 %in% codelist$iso2c[!is.na(codelist$eu28)]) %>%
  filter(month(date) == 7) %>%
  mutate(ship_insurer_country =
           case_when(ship_insurer_country %in% ship_insurer_ranking$ship_insurer_country[1:5] ~ ship_insurer_country,
                     EU~'Other EU', T~'Others') %>% na.cover('Unknown')) %>%
  group_by(commodity, ship_insurer_country, date) %>% summarise(across(value_tonne, sum)) %>%
  group_by(commodity, ship_insurer_country) %>% summarise(across(value_tonne, mean)) %>%
  mutate(ship_insurer_country =
           factor(ship_insurer_country,
                  levels=rev(c(ship_insurer_ranking$ship_insurer_country[1:2], 'Other EU',
                               ship_insurer_ranking$ship_insurer_country[3:5], 'Others', 'Unknown')))) %>%
  write_csv(paste0(output_dir,'Ship capacity carrying Russian fossil fuels in July, by insurer.csv')) %>%
  group_by(commodity) %>%
  mutate(share = value_tonne / sum(value_tonne)) ->
  capacity_plot_insurer


capacity_plot_insurer %>% group_by(ship_insurer_country) %>% summarise(across(value_tonne, sum)) %>%
  mutate(share = value_tonne / sum(value_tonne))

capacity_plot_insurer %>%
  ggplot(aes(commodity, value_tonne/1e6, fill=ship_insurer_country)) + geom_col() +
  geom_text(aes(label=scales::percent(share, accuracy=1), alpha=ifelse(share<.1, 0, 1)),
            position = position_stack(vjust = .5),
            col='white') +
  theme_crea() + scale_fill_crea_d() +
  labs(title='UK and Norway insurers cover most Russian fossil fuel shipments',
       subtitle='verage ship capacity loaded with Russian fossil fuels in July, by country of insurer',
       y='deadweight tonnage, million, average',
       fill='Ship insurer country') +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  scale_alpha(guide='none', range = c(0,1))
ggsave(paste0(output_dir,'Ship capacity carrying Russian fossil fuels, by insurer.png'),
       width=8,height=6)
