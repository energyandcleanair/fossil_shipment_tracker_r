

# Only actual observed pipeline flows and shipments
counter_last <- read_csv("https://api.russiafossiltracker.com/v0/counter_last?aggregate_by=destination_region,commodity_group&date_from=2022-02-24&format=csv")

counter_wo_estimates <- read_csv("https://api.russiafossiltracker.com/v0/counter?aggregate_by=destination_region,commodity_group&fill_with_estimates=False&date_from=2022-02-24&format=csv")

# This includes filling last days with last 7-day average (after removing last 2 days)
counter_w_estimates <- read_csv("https://api.russiafossiltracker.com/v0/counter?aggregate_by=destination_region,commodity&fill_with_estimates=True&date_from=2022-02-24&format=csv")


counter_last %>%
  filter(destination_region=="EU28")

counter_wo_estimates %>%
  filter(destination_region=="EU28") %>%
  group_by(destination_region) %>%
  summarise(value_bneur=sum(value_eur)/1e9)

counter_w_estimates %>%
  filter(destination_region=="EU28") %>%
  group_by(destination_region, commodity) %>%
  summarise(value_bneur=sum(value_eur, na.rm=T)/1e9)


counter_w_estimates %>%
  # group_by(destination_region) %>%
  summarise(value_bneur=sum(value_eur,na.rm=T)/1e9)

counter_w_estimates <- read_csv("https://api.russiafossiltracker.com/v0/counter?aggregate_by=destination_region,commodity&fill_with_estimates=True&date_from=2022-02-24&format=csv")

counter_w_estimates %>%
  group_by(destination_region) %>%
  summarise(value_bneur=sum(value_eur,na.rm=T)/1e9)

counter_w_estimates %>%
  summarise(value_bneur=sum(value_eur,na.rm=T)/1e9)

counter_w_estimates %>%
  group_by(destination_region,commodity) %>%
  summarise(value_bneur=sum(value_eur,na.rm=T)/1e9)




# Comparison with comtrade/eurostat ---------------------------------------


library(tidyverse)
library(countrycode)
library(russiacounter)

comtrade_eurostat <- comtrade_eurostat.get_flows(use_cache=T)

commodities <- c("crude_oil","oil_products", "lng", "coal")
voyages <-read_csv("https://fossil-shipment-tracker.ew.r.appspot.com/v0/voyage?aggregate_by=departure_date,destination_region,departure_country,commodity,status&format=csv&date_from=2021-12-01")
# departures <-read_csv("https://fossil-shipment-tracker.ew.r.appspot.com/v0/departure?format=csv&date_from=2021-12-01")
# ports <-read_csv("https://fossil-shipment-tracker.ew.r.appspot.com/v0/port?format=csv&date_from=2021-12-01")




d <- voyages %>%
  filter(departure_iso2 == "RU") %>%
  # filter(destination_region=="EU28") %>%
  filter(status %in% c("completed"))  %>%
  # filter(status!='undetected_arrival') %>%
  filter(commodity %in% commodities) %>%
  group_by(commodity, date=departure_date) %>%
  summarise(value=sum(value_tonne, na.rm=T)) %>%
  ungroup() %>%
  tidyr::complete(date, nesting(commodity),
                  fill = list(value = 0))

d_comparison <- comtrade_eurostat %>%
  mutate(commodity=recode(commodity,
                          "oil"="crude_oil",
                          "oil_others"="oil_products")) %>%
  filter(
    country!="EU",
         partner=="Russia",
         unit %in% c("tonne"),
         commodity %in% commodities) %>%
  group_by(date, commodity, partner, unit, source) %>%
  summarise(value=sum(value, na.rm=T)) %>%
  rename(month=date) %>%
  left_join(
    tibble(date=seq(min(comtrade_eurostat$date), max(comtrade_eurostat$date) + lubridate::days(31), by="day")) %>%
      mutate(weight=1/lubridate::days_in_month(date),
             month=lubridate::floor_date(date, "month"))) %>%
  mutate(value=value*weight) %>%
  filter(date>="2021-12-01")

d7 <- bind_rows(d %>% mutate(source="shipments"),
                d_comparison) %>%
  rcrea::utils.running_average(7)

ggplot(d7) +
  geom_line(aes(date, value, col=source)) +
  facet_wrap(~commodity) +
  labs(y="tonne / day", x=NULL, subtitle='7-day running average')



# Switching to new counter ------------------------------------------------

voyages <- read_csv("https://api.russiafossiltracker.com/v0/voyage?date_from=2022-02-24&aggregate_by=arrival_date,destination_region,status,commodity_group&format=csv")
overland_flows <- read_csv("https://api.russiafossiltracker.com/v0/overland?date_from=2022-02-24&aggregate_by=date,destination_region,commodity_group&format=csv")


voyages %>%
  filter(destination_region != 'Russia',
         status %in% c('completed', 'ongoing'),
         !is.na(commodity_group)) %>%
  rename(date=arrival_date) %>%
  bind_rows(overland_flows) %>%
  filter(date>'2022-02-24') %>%
  group_by(destination_region, commodity_group) %>%
  summarise(across(c(value_tonne, value_eur), sum, na.rm=T)) %>%
  mutate(value_bneur = value_eur / 1e9) %>%
  filter(destination_region=="EU28")
