library(tidyverse)
library(rcrea)
library(lubridate)
library(ggplot2)

dir.create('scripts/price_cap', F, T)

# To reduce query size beneath limit, we select only required fields
shipment_fields <- 'id,commodity_origin_iso2,commodity_destination_iso2,value_eur,value_tonne,arrival_date_utc,commodity,commodity_group,commodity_destination_region,destination_iso2,ship_owner_iso2,ship_owner_region,ship_insurer_region,ship_insurer_iso2,pricing_scenario'

shipments <- read_csv(sprintf('https://api.russiafossiltracker.com/v0/voyage?pricing_scenario=default&format=csv&date_from=2021-12-01&arrival_date_to=2022-09-30&select=%s', shipment_fields))
overland <- read_csv('https://api.russiafossiltracker.com/v0/overland?keep_zeros=False&format=csv&date_from=2021-12-01&date_to=2022-09-30')


price_caps <- bind_rows(
  price.get_capped_prices(version='2021H1') %>% mutate(scenario='pricecap_2021H1'),
  price.get_capped_prices(version='andrei') %>% mutate(scenario='pricecap_andrei')
  ) %>%
  filter(date >= min(shipments$arrival_date_utc),
         date <= max(shipments$arrival_date_utc))

price_caps %>%
  filter(country_iso2=='DE') %>%
  ggplot() + geom_line(aes(date, eur_per_tonne, col=scenario)) + facet_wrap(~commodity)


# Apply price cap only to right shipments and pipeline --------------------
# In Python, pricecap is applied regardless of ship owners or insurers or even destination
# So here we remove the pricecap from shipments and pipelines that shouldn't be pricecapped

counter_manual_shipments <- shipments %>%
  filter(!is.na(value_eur)) %>%
  filter(commodity_origin_iso2=='RU',
         commodity_destination_iso2!='RU') %>%
  select(id, date=arrival_date_utc, commodity, commodity_group, destination_region=commodity_destination_region, destination_iso2, ship_owner_iso2, ship_owner_region, ship_insurer_region,
         ship_insurer_iso2, value_eur, value_tonne) %>%
  mutate(date=floor_date(date, 'day')) %>%
  mutate(default=value_eur/value_tonne) %>%
  select(-c(value_eur)) %>%
  left_join(
    price_caps %>%
      tidyr::spread(scenario, eur_per_tonne) %>%
      rename(destination_iso2=country_iso2)
  ) %>%
  # should_apply_pricecap_1: EU imports
  # should_apply_pricecap_2: EU imports + EU/GB/NO insured/owned ships
  mutate(should_apply_pricecap_1=
           (date>='2022-07-01') & # Theoretically already applied in Python
           ((!is.na(destination_region) & (destination_region=='EU' | destination_iso2  %in% c('UK','GB','NO')))),
         should_apply_pricecap_2= (date>='2022-07-01') & (should_apply_pricecap_1 |
           ((!is.na(ship_owner_iso2) & (ship_owner_region=='EU' | ship_owner_iso2 %in% c('UK','GB','NO'))) |
              (!is.na(ship_insurer_iso2) & (ship_insurer_region=='EU' | ship_insurer_iso2 %in% c('UK','GB','NO')))))) %>%
  mutate(pricecap_2021H1_1=ifelse(should_apply_pricecap_1, pmin(pricecap_2021H1, default, na.rm=T), default),
         pricecap_2021H1_2=ifelse(should_apply_pricecap_2, pmin(pricecap_2021H1, default, na.rm=T), default),
         pricecap_andrei_1=ifelse(should_apply_pricecap_1, pmin(pricecap_andrei, default, na.rm=T), default),
         pricecap_andrei_2=ifelse(should_apply_pricecap_2, pmin(pricecap_andrei, default, na.rm=T), default)
         ) %>%
  select(-c(pricecap_2021H1, pricecap_andrei)) %>%
  tidyr::gather('pricing_scenario', 'eur_per_tonne', default, pricecap_2021H1_1, pricecap_2021H1_2, pricecap_andrei_1, pricecap_andrei_2) %>%
  mutate(value_eur = value_tonne * eur_per_tonne)


counter_manual_overland <- overland %>%
  filter(commodity_origin_iso2=='RU') %>%
  select(id, date, commodity, commodity_group, destination_iso2=commodity_destination_iso2, destination_region=commodity_destination_region, value_eur, value_tonne) %>%
  mutate(default=value_eur/value_tonne) %>%
  select(-c(value_eur)) %>%
  left_join(
    price_caps %>%
      tidyr::spread(scenario, eur_per_tonne) %>%
      rename(destination_iso2=country_iso2)
  ) %>%
  mutate(should_apply_pricecap=
           (date>='2022-07-01') &  # Theoretically already applied in Python
           (destination_region == 'EU')) %>%
  mutate(pricecap_2021H1_1=ifelse(should_apply_pricecap, pmin(pricecap_2021H1, default, na.rm=T), default),
         pricecap_2021H1_2=ifelse(should_apply_pricecap, pmin(pricecap_2021H1, default, na.rm=T), default),
         pricecap_andrei_1=ifelse(should_apply_pricecap, pmin(pricecap_andrei, default, na.rm=T), default),
         pricecap_andrei_2=ifelse(should_apply_pricecap, pmin(pricecap_andrei, default, na.rm=T), default)
  ) %>%
  select(-c(pricecap_2021H1, pricecap_andrei)) %>%
  tidyr::gather('pricing_scenario', 'eur_per_tonne', default, pricecap_2021H1_1, pricecap_2021H1_2, pricecap_andrei_1, pricecap_andrei_2) %>%
  mutate(value_eur = value_tonne * eur_per_tonne)




counter_manual <-
  bind_rows(counter_manual_shipments,
            counter_manual_overland) %>%
  select(date, destination_region, commodity, commodity_group, pricing_scenario, value_eur) %>%
  mutate(date=as.Date(date)) %>%
  # tidyr::gather(key='pricing_scenario', value='value_eur', -c(date, commodity, commodity_group, destination_region)) %>%
  group_by(date, pricing_scenario, destination_region, commodity, commodity_group) %>%
  summarise(value_eur=sum(value_eur)) %>%
  ungroup()


# Diagnostic plot
counter_manual %>%
  select(date, destination_region, commodity, commodity_group, pricing_scenario, value_eur) %>%
  group_by(date, commodity, pricing_scenario) %>%
  summarise(value_eur=sum(value_eur, na.rm=T)) %>%
  rcrea::utils.running_average(14, vars_to_avg = 'value_eur') %>%
  ggplot() + geom_line(aes(date, value_eur, col=pricing_scenario)) +
  facet_wrap(~commodity)


# Remove coal after coal ban, pipeline lng and pipeline oil to eu
counter_manual <-
  counter_manual %>%
  mutate(value_eur = replace(value_eur, date >= '2022-08-11'
                             & commodity == 'coal'
                             & destination_region == 'EU', 0)) %>%
  mutate(value_eur = if_else(date >= '2022-06-06'
                             & commodity == 'lng_pipeline', 0, value_eur))






# Setting price caps on Russian fossil fuels could have cut the EU’s import bills by EUR XX billion since the beginning of July, when the measure was first discussed at a high level at the G7 Summit.
(diff_counter <- counter_manual %>%
    filter(date>='2022-07-01',
           destination_region=='EU') %>%
    group_by(destination_region, pricing_scenario) %>%
    summarise(value_bneur=sum(value_eur, na.rm=T)/1e9) %>%
    tidyr::spread(pricing_scenario, value_bneur))


# Russia’s revenues from fossil fuel exports could have been slashed by XX billion, if the price caps applied to all fossil fuel cargoes carried to third countries aboard European-owned or insured ships, in addition to imports into the Union.
(counter_manual %>%
  filter(date >= '2022-02-24') %>%
  group_by(pricing_scenario) %>%
  summarise(value_bneur=sum(value_eur)/1e9) %>%
  tidyr::spread(pricing_scenario, value_bneur) %>%
  mutate(diff2=default-pricecap2))


scenarios <- list(
  default = 'Without price cap',
  pricecap_2021H1_1 = 'Pricecap: 2021H1 - EU imports only',
  pricecap_2021H1_2 = 'Pricecap: 2021H1 - EU imports + EU/GB/NO ships',
  pricecap_andrei_1 = 'Pricecap: MCOP - EU imports only',
  pricecap_andrei_2 = 'Pricecap: MCOP - EU imports + EU/GB/NO ships'
)

scenarios_short <- list(
  default = 'Without price cap',
  pricecap_2021H1_1 = 'With 2021H1 price cap',
  pricecap_2021H1_2 = 'With 2021H1 price cap',
  pricecap_andrei_1 = 'With MCOP price cap',
  pricecap_andrei_2 = 'With MCOP price cap'
)




# Plots -------------------------------------------------------------------
lapply(c(7, 14, 30), function(running_days){

  d <- counter_manual %>%
    filter(pricing_scenario %in% c('default', 'pricecap_2021H1_2', 'pricecap_andrei_2')) %>%
    group_by(pricing_scenario, date) %>%
    summarise(value_eur=sum(value_eur)) %>%
    mutate(pricing_scenario=recode(pricing_scenario, !!!scenarios_short)) %>%
    filter(!is.na(date)) %>%
    rcrea::utils.running_average(running_days, vars_to_avg = 'value_eur')

  ggplot(d) +
    geom_line(aes(date, value_eur/1e6, col=pricing_scenario), show.legend = F) +
    rcrea::theme_crea() +
    labs(y='million EUR per day',
         x=NULL,
         title='Potential impact of price cap on Russia\'s fossil fuel revenues',
         subtitle = sprintf('%d-day running average',running_days),
         color=NULL,
         caption=paste0('MCOP: price cap based on Marginal Cost of Production.\n',
                         '2021H1: price cap based on average prices in the first half of 2021.\n',
                         'Source: CREA analysis. Assuming a price cap starting on 1 July 2022 on EU imports and EU+UK+NO owned or insured ships.')) +
    scale_x_date(limits=c(as.Date('2022-01-01'), max(d$date) + lubridate::days(60))) +
    expand_limits(y=0) +
    scale_y_continuous(expand=expansion(mult=c(0,.05))) +
    rcrea::scale_color_crea_d(palette='dramatic') +
    theme(legend.position = 'bottom') +
    guides(color=guide_legend(nrow = 1)) +
    ggrepel::geom_text_repel(data=d %>% filter(date==max(date)),
                             aes(date, value_eur/1e6,
                                 label=sprintf('%s\nEUR %dmn / day', pricing_scenario,
                                               round(value_eur/1e6)),
                                 col=pricing_scenario),
                             show.legend = F,
                             size=2.5,
                             # direction='x',
                             fontface='bold',
                             hjust=0,
                             lineheight=1,
                             nudge_x=10,
                             segment.colour = NA
                             # min.segment.length=5
                             )

  ggsave(sprintf('scripts/price_cap/impact_pricecap_%dd.png', running_days), width=8, height=5)
})


# Data --------------------------------------------------------------------
counter_manual %>%
  # filter(pricing_scenario %in% c('default', 'pricecap_2021H1_2', 'pricecap_andrei_2')) %>%
  group_by(date, pricing_scenario, destination_region, commodity_group) %>%
  summarise(value_eur=sum(value_eur)) %>%
  mutate(pricing_scenario=recode(pricing_scenario, !!!scenarios)) %>%
  tidyr::spread(pricing_scenario, value_eur) %>%
  # rename(no_price_cap=default,
  #        pricecap_2021on_eu_imports=pricecap1,
  #        pricecap_on_eu_imports_and_ships=pricecap2) %>%
  mutate(unit='EUR') %>%
  write_csv('scripts/price_cap/price_cap_ts.csv')
