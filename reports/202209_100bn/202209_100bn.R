library(tidyverse)

counter <- read_csv('https://api.russiafossiltracker.com/v0/counter?pricing_scenario=default,pricecap&format=csv&date_from=2021-12-01&date_to=-7')
shipments <- read_csv('https://api.russiafossiltracker.com/v0/voyage?pricing_scenario=default,pricecap&format=csv&date_from=2022-07-01')


# Save a version for when report will be released
if(T){
  dir.create('reports/202209_100bn/cache',F,T)
  saveRDS(counter, 'reports/202209_100bn/cache/counter.RDS')
  saveRDS(shipments, 'reports/202209_100bn/cache/shipments.RDS')
}


# While coal imports from Russia to Europe have stopped completely and gas imports have contracted dramatically, the EU continues to import crude oil and oil products worth approximately EUR XX million per day
start_date <- as.Date('2022-02-24')
last_date <-  max(counter$date)
period_length <- lubridate::days(14)

counter %>%
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
(diff_counter <- counter %>%
  filter(date>='2022-07-01',
         date<='2022-09-30',
         destination_region=='EU') %>%
  group_by(destination_region, pricing_scenario) %>%
  summarise(value_bneur=sum(value_eur)/1e9) %>%
  tidyr::spread(pricing_scenario, value_bneur) %>%
  mutate(diff=default-pricecap))


# Russia’s revenues from fossil fuel exports could have been slashed by XX billion, if the price caps applied to all fossil fuel cargoes carried to third countries aboard European-owned or insured ships, in addition to imports into the Union.
shipments %>%
  filter(arrival_date_utc>='2022-07-01',
         arrival_date_utc<='2022-09-30',
         (ship_owner_region=='EU' | ship_insurer_region=='EU'),
            # ship_owner_iso2 %in% c('UK','GB','NO') | ship_insurer_iso2 %in% c('UK','GB','NO')),
         commodity_destination_region != 'EU') %>%
  filter(!is.na(value_eur)) %>%
  group_by(commodity_destination_region, pricing_scenario) %>%
  summarise(value_bneur=sum(value_eur)/1e9) %>%
  tidyr::spread(pricing_scenario, value_bneur) %>%
  mutate(diff=default-pricecap) %>%
  pull(diff) %>%
  sum()


# Plots -------------------------------------------------------------------
d <- counter %>%
  group_by(pricing_scenario, date) %>%
  summarise(value_eur=sum(value_eur)) %>%
  rcrea::utils.running_average(30, vars_to_avg = 'value_eur') %>%
  mutate(pricing_scenario=factor(pricing_scenario,
                                 levels=rev(c('default', 'pricecap')),
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
  rcrea::scale_y_crea_zero() +
  rcrea::scale_color_crea_d() +
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

ggsave('reports/202209_100bn/impact_pricecap.jpg', width=8, height=5)


