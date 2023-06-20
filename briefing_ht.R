voyages <- read_csv("https://api.russiafossiltracker.com/v0/voyage?aggregate_by=arrival_date,destination_country,status,commodity&format=csv")
overland_flows <- read_csv("https://api.russiafossiltracker.com/v0/overland?aggregate_by=date,destination_country,commodity&format=csv")


recode_commodity <- function(df){
  df %>% mutate(commodity = recode(commodity,
                            coal="Coal",
                            lng="LNG",
                            lng_pipeline="LNG",
                            crude_oil="Crude oil",
                            crude_oil_rail_road="Crude oil",
                            natural_gas="Pipeline gas",
                            oil_products="Oil products",
                            oil_products_pipeline="Oil products",
                            oil_products_rail_road="Oil products",
                            pipeline_oil="Crude oil",
                            .default="Others"
                            )) %>%
    mutate(commodity = factor(commodity, levels=rev(c("Pipeline gas", "Crude oil", "LNG", "Oil products", "Coal", "Others"))))
}

library(rcrea)

voyages %>%
  rename(date=arrival_date) %>%
  filter(destination_country != 'Russia',
         !commodity %in% c('bulk_not_coal', 'general_cargo'),
         status %in% c('completed', 'ongoing')) %>%
  bind_rows(overland_flows) %>%
  recode_commodity() %>%
  filter(date>'2022-02-24') %>%
  group_by(destination_country, destination_region, commodity, status) %>%
  summarise(across(c(value_tonne, value_eur), sum, na.rm=T)) -> plotdata

plotdata %>%
  group_by(destination_country) %>%
  summarise(across(c(value_tonne, value_eur), sum)) %>%
  arrange(-value_eur) -> country_ranking

#print total by top country
country_ranking %>%
  mutate(statement=paste0(destination_country, ' (EUR', signif(value_eur/1e9, 2), 'bln)')) %>%
  use_series(statement) %>% head(5) %>% paste(collapse=', ')


voyages %>% filter(arrival_date>='2022-02-24', status=='completed', destination_country != 'Russia') %>%
  use_series(value_eur) %>% sum(na.rm=T) %>% divide_by(1e9)

#print total overland
overland_flows$value_eur %>% sum(na.rm=T) %>% divide_by(1e9)

plotdata %>% ungroup %>%
  group_by(EU=destination_region=='EU28', commodity) %>%
  summarise(across(c(value_tonne, value_eur), sum, na.rm=T)) ->
  totals

totals %<>% group_by(EU) %>% summarise(across(c(value_tonne, value_eur), sum, na.rm=T)) %>%
  mutate(commodity='total') %>% bind_rows(totals)

#print EU share of total by commodity
totals %>%
  group_by(commodity) %>%
  mutate(share = value_eur / sum(value_eur, na.rm=T),
         statement = paste(scales::percent(share, accuracy=1), 'for', commodity)) %>% filter(EU) %>%
  use_series(statement) %>% paste(collapse=', ')

plotdata %>%
  mutate(destination_country = factor(destination_country, levels=rev(country_ranking$destination_country))) %>%
  filter(destination_country %in% country_ranking$destination_country[1:20], !is.na(destination_country)) %>%
  ggplot(aes(destination_country, value_eur/1e6, fill=commodity)) + geom_col() +
  #facet_wrap(~status) +
  coord_flip() +
  theme_crea() +
  labs(title='Largest importers of fossil fuels from Russia',
       subtitle = 'in the first two months of the invasion',
       y='mln EUR', x='') +
  scale_fill_crea_d('dramatic', col.index = c(5,4,1,6,2,3)) +
  scale_y_continuous(expand=expansion(mult=c(0,.05)))



