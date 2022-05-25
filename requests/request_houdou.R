library(tidyverse)

shipments <- read_csv("https://api.russiafossiltracker.com/v0/voyage?format=csv&date_from=2022-02-24&aggregate_by=departure_iso2,destination_iso2,commodity")

pipeline <- read_csv("https://api.russiafossiltracker.com/v0/overland?format=csv&date_from=2022-02-24&aggregate_by=departure_iso2,destination_iso2,commodity")


imports <- bind_rows(pipeline %>% select(departure_iso2, destination_iso2, destination_country, commodity_group, value_eur) %>%
                       filter(departure_iso2 %in% c("RU","BY","TR")),
                     shipments %>% select(departure_iso2, destination_iso2, destination_country, commodity_group, value_eur) %>%
                       filter(departure_iso2 %in% c("RU"))
                     ) %>%
  filter(!is.na(commodity_group)) %>%
  group_by(destination_country, commodity_group) %>%
  summarise(value_eur=round(sum(value_eur, na.rm=T))) %>%
  ungroup() %>%
  tidyr::pivot_wider(names_from="commodity_group",
                     values_from="value_eur") %>%
  mutate(total=gas+oil+coal,
         unit="EUR") %>%
  arrange(desc(total))

imports %>%
  head(20) %>%
  write_csv("requests/top20_importers.csv")

