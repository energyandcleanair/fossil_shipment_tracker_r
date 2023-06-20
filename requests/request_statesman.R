library(russiacounter)
library(tidyverse)
library(lubridate)

flows_eurostat_exeu = db.download_flows("eurostat_exeu")
flows_comtrade_eurostat = db.download_flows("comtrade_eurostat")
flows_comtrade_eurostat_2022 = utils.expand_in_2022(flows_comtrade_eurostat, flows_eurostat_exeu)
flows_entsog = db.download_flows("entsog")

prices <- price.get_modelled_price(flows_entsog=flows_entsog,
                                   flows_comtrade_eurostat=flows_comtrade_eurostat_2022)


write_csv(prices, "requests/daily_flows.csv")


prices %>%
  ungroup() %>%
  filter(date >= "2022-02-24",
         date <= "2022-03-23") %>%
  summarise(value_eur=sum(value_eur, na.rm=T))


max(prices$date)
