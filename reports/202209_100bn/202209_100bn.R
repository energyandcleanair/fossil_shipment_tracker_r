library(tidyverse)
library(rcrea)
library(lubridate)
library(ggplot2)

# counter <- read_csv('https://api.russiafossiltracker.com/v0/counter?pricing_scenario=default,pricecap&format=csv&date_from=2021-12-01&date_to=-7')
shipments <- read_csv('https://api.russiafossiltracker.com/v0/voyage?pricing_scenario=default,pricecap&format=csv&date_from=2021-12-01&date_to=-7')
overland <- read_csv('https://api.russiafossiltracker.com/v0/overland?pricing_scenario=default,pricecap&keep_zeros=False&format=csv&date_from=2021-12-01&date_to=-7')



# Apply price cap only to right shipments and pipeline --------------------
# In Python, pricecap is applied regardless of ship owners or insurers or even destination
# So here we remove the pricecap from shipments and pipelines that shouldn't be pricecapped

counter_manual_shipments <-
  shipments %>%
    filter(!is.na(value_eur)) %>%
    filter(commodity_origin_iso2=='RU',
           commodity_destination_iso2!='RU') %>%
    select(id, date=arrival_date_utc, commodity, destination_region=commodity_destination_region, destination_iso2, ship_owner_iso2, ship_owner_region, ship_insurer_region,
           ship_insurer_iso2, value_eur, pricing_scenario) %>%
    mutate(date=floor_date(date, 'day')) %>%
    tidyr::spread(pricing_scenario, value_eur) %>%
    mutate(should_apply_pricecap_1=
             (date>='2022-07-01') & # Theoretically already applied in Python
             (date<='2022-09-30') & # Theoretically already applied in Python
             ((!is.na(destination_region) & (destination_region=='EU' | destination_iso2  %in% c('UK','GB','NO'))))) %>%
  mutate(pricecap1=ifelse(should_apply_pricecap_1, pricecap, default)) %>%
  mutate(pricecap2=ifelse(should_apply_pricecap_1 | ((!is.na(ship_owner_iso2) & (ship_owner_region=='EU' | ship_owner_iso2 %in% c('UK','GB','NO'))) |
                            (!is.na(ship_insurer_iso2) & (ship_insurer_region=='EU' | ship_insurer_iso2 %in% c('UK','GB','NO')))),pricecap,default))


counter_manual_overland <- overland %>%
  filter(commodity_origin_iso2=='RU') %>%
  select(id, date, commodity, destination_region=commodity_destination_region, value_eur, pricing_scenario) %>%
  tidyr::spread(pricing_scenario, value_eur) %>%
  mutate(should_apply_pricecap=
           (date>='2022-07-01') &  # Theoretically already applied in Python
           (date<='2022-09-30') &  # Theoretically already applied in Python
           (destination_region == 'EU')) %>%
  mutate(pricecap1=ifelse(should_apply_pricecap, pricecap, default)) %>%
  mutate(pricecap2=pricecap1)


counter_manual <-
  bind_rows(counter_manual_shipments,
            counter_manual_overland) %>%
  select(date, default, destination_region, commodity, pricecap1, pricecap2) %>%
  mutate(date=as.Date(date)) %>%
  tidyr::gather(key='pricing_scenario', value='value_eur', -c(date, commodity, destination_region)) %>%
  group_by(date, pricing_scenario, destination_region, commodity) %>%
  summarise(value_eur=sum(value_eur))

# Remove coal after coal ban, pipeline lng and pipeline oil to eu
counter_manual <-
  counter_manual %>%
  mutate(value_eur = replace(value_eur, date >= '2022-08-11'
                             & commodity == 'coal'
                             & destination_region == 'EU', 0)) %>%
  mutate(value_eur = replace(value_eur, date >= '2022-09-01'
                             & commodity == 'pipeline_oil'
                             & destination_region == 'EU', 0)) %>%
  mutate(value_eur = if_else(date >= '2022-06-06'
                             & commodity == 'lng_pipeline', 0, value_eur))

# Save a version for when report will be released
if(T){
  dir.create('reports/202209_100bn/cache',F,T)
  saveRDS(counter_manual, 'reports/202209_100bn/cache/counter_manual.RDS')
  saveRDS(shipments, 'reports/202209_100bn/cache/shipments.RDS')
  saveRDS(overland, 'reports/202209_100bn/cache/overland')
}


# While coal imports from Russia to Europe have stopped completely and gas imports have contracted dramatically, the EU continues to import crude oil and oil products worth approximately EUR XX million per day
start_date <- as.Date('2022-02-24')
last_date <-  max(counter_manual$date)
period_length <- lubridate::days(14)

counter_manual %>%
  filter(commodity != 'coal',
         destination_region == 'EU') %>%
  mutate(period=case_when(
    (date >= last_date - period_length) & (date <= last_date) ~ 'most_recent',
    (date >= start_date - period_length) & (date <= start_date) ~ 'before'
  )) %>%
  filter(!is.na(period)) %>%
  group_by(period, date) %>%
  summarise(value_mneur=sum(value_eur/1e6)) %>%
  summarise(value_mneur=mean(value_mneur)) %>%
  tidyr::spread(period, value_mneur) %>%
  mutate(diff_pct=1 - most_recent/before)



# Setting price caps on Russian fossil fuels could have cut the EU’s import bills by EUR XX billion since the beginning of July, when the measure was first discussed at a high level at the G7 Summit.
(diff_counter <- counter_manual %>%
  filter(date>='2022-07-01',
         date<='2022-09-30',
         destination_region=='EU') %>%
  group_by(destination_region, pricing_scenario) %>%
  summarise(value_bneur=sum(value_eur)/1e9) %>%
  tidyr::spread(pricing_scenario, value_bneur) %>%
  mutate(diff=default-pricecap1))


# Russia’s revenues from fossil fuel exports could have been slashed by XX billion, if the price caps applied to all fossil fuel cargoes carried to third countries aboard European-owned or insured ships, in addition to imports into the Union.
counter_manual %>%
  filter(date >= '2022-02-24') %>%
  group_by(pricing_scenario) %>%
  summarise(value_bneur=sum(value_eur)/1e9) %>%
  tidyr::spread(pricing_scenario, value_bneur) %>%
  mutate(diff=default-pricecap2) %>%
  pull(diff) %>%
  sum()


# Plots -------------------------------------------------------------------
d <- counter_manual %>%
  filter(pricing_scenario %in% c('default', 'pricecap2')) %>%
  group_by(pricing_scenario, date) %>%
  summarise(value_eur=sum(value_eur)) %>%
  rcrea::utils.running_average(30, vars_to_avg = 'value_eur') %>%
  mutate(pricing_scenario=factor(pricing_scenario,
                                 levels=rev(c('default', 'pricecap2')),
                                 labels=rev(c('Without price cap', 'With price cap'))))
ggplot(d) +
  geom_line(aes(date, value_eur/1e6, col=pricing_scenario)) +
  rcrea::theme_crea() +
  labs(y='million EUR per day',
       x=NULL,
       title='Potential impact of price cap on Russia\'s fossil fuel revenues',
       subtitle = '30-day running average',
       color=NULL,
       caption='Source: CREA analysis. Assuming a price cap starting on 1 July 2022, set with 2021H1 global prices.') +
  scale_x_date(limits=c(as.Date('2022-01-01'), max(d$date) + lubridate::days(60))) +
  expand_limits(y=0) +
  scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  theme(legend.position = 'bottom') +
  guides(color=guide_legend(nrow = 1)) +
  ggrepel::geom_text_repel(data=d %>% filter(date==max(date)),
            aes(date, value_eur/1e6,
                label=sprintf('%s\nEUR %dmn / day', pricing_scenario,
                              round(value_eur/1e6)),
                col=pricing_scenario),
            show.legend = F,
            direction='x',
            fontface='bold',
            hjust=0,
            nudge_x=5)

ggsave('reports/202209_100bn/impact_pricecap.jpg', width=8, height=4)


