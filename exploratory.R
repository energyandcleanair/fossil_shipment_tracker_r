
# Eurostat ----------------------------------------------------------------

eurostat.germany <- eurostat::get_eurostat("nrg_ti_gasm") %>%
  filter(geo=="DE")

eurostat.germany %>%
  mutate(partner=countrycode::countrycode(partner, "iso2c", "country.name", custom_match = c("UK"="United Kingdom", "EL"="Greece", "XK"="XK", "TOTAL"="TOTAL"))) %>%
  group_by(partner, siec, partner, unit, year=lubridate::year(time)) %>%
  summarise(value=sum(values)) %>%
  filter(value>0) %>%
  ggplot() +
  geom_bar(stat="identity", aes(value, partner, fill=siec), position="dodge") +
  facet_wrap(~unit, scales="free_x")


eurostat.italy <- eurostat::get_eurostat("nrg_ti_gasm") %>%
  filter(geo=="IT")

eurostat.italy %>%
  mutate(partner=countrycode::countrycode(partner, "iso2c", "country.name", custom_match = c("UK"="United Kingdom", "EL"="Greece", "XK"="XK", "TOTAL"="TOTAL"))) %>%
  group_by(partner, siec, partner, unit, year=lubridate::year(time)) %>%
  summarise(value=sum(values)) %>%
  filter(value>0) %>%
  ggplot() +
  geom_bar(stat="identity", aes(value, partner, fill=siec), position="dodge") +
  facet_wrap(~unit, scales="free_x")


eurostat.austria <- eurostat::get_eurostat("nrg_ti_gasm") %>%
  filter(geo=="AT")

eurostat.austria %>%
  mutate(partner=countrycode::countrycode(partner, "iso2c", "country.name", custom_match = c("UK"="United Kingdom", "EL"="Greece", "XK"="XK", "TOTAL"="TOTAL"))) %>%
  group_by(partner, siec, partner, unit, year=lubridate::year(time)) %>%
  summarise(value=sum(values)) %>%
  filter(value>0) %>%
  ggplot() +
  geom_bar(stat="identity", aes(value, partner, fill=siec), position="dodge") +
  facet_wrap(~unit, scales="free_x")
