# Comparing with IEA Gas Trade Flow data, available at: https://www.iea.org/data-and-statistics/data-product/gas-trade-flows




# library(remotes)
# remotes::install_github("energyandcleanair/entsog")

library(entsog)
library(tidyverse)
library(readxls)

source('russia.R')


entso <- get_russia_exports(use_cache=T) %>%
  filter(unit=="kWh/d") %>%
  left_join(operators %>% distinct(operatorKey, operatorCountryLabel)) %>%
  group_by(month=lubridate::floor_date(date,"month"), to=operatorCountryLabel, unit) %>%
  summarise_at("value", sum, na.rm=T) %>%
  # Now it is in kwh/month
  mutate(value = value * MJ_per_kWh / gcv_MJ_m3 / 24 / lubridate::days_in_month(month),
         unit="m3/h") %>%
  ungroup() %>%
  mutate(from="Russia",
         source="ENTSO (CREA)") %>%
  select(from, to, month, value, unit, source)

iea <- readxl::read_xls("data/iea/Export_GTF_IEA_202112.xls") %>%
  filter(Exit=="Russia") %>%
  tidyr::pivot_longer(cols=-c(Borderpoint, Exit, `...3`, Entry,	`MAXFLOW (Mm3/h)`),
                      values_transform=list(value=as.numeric),
                      names_to="month") %>%
  mutate(value=value*1E6,
         unit="m3/month",
         month=lubridate::floor_date(as.Date(as.numeric(month), origin = "1900-01-01"), 'month')) %>%
  mutate(value=value / lubridate::days_in_month(month) / 24,
         unit="m3/h") %>%
  select(from=Exit,
         to=Entry,
         month,
         value,
         unit) %>%
  mutate(source="IEA Gas Trade Flow")


bind_rows(iea, entso) %>%
  filter(month>="2017-01-01",
         to!="Turkey") %>%
  mutate(value=value/1E6,
         unit="Mm3/h") %>%
  ggplot() +
  geom_line(aes(month, value, col=source), position="dodge", stat="identity") +
  facet_wrap(~to, scales="free_y") +
  scale_y_continuous(limits=c(0,NA), expand = expansion(mult=c(0,0.1))) +
  labs(y="Mm3/h",
       x=NULL) +
  theme_light()



entso_twh_2021 <- get_russia_exports(use_cache=T) %>%
  filter(lubridate::year(date)==2021) %>%
  summarise(value=sum(value, na.rm=T)/1E9) # ~2120TWh (Bruege says 1550)


# Nordstream
get_russia_exports() %>%
  filter(grepl("Greifswald", pointLabel),
         unit=="kWh/d") %>%
  mutate(value=value * MJ_per_kWh / gcv_MJ_per_m3,
         unit="m3/day") %>%
  ungroup() %>%
  summarise(value=sum(value, na.rm=T)) %>%
  mutate(value=value/1e9,
         unit="bcm/yr")

