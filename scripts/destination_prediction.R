library(tidyverse)

v <- read_csv('https://api.russiafossiltracker.com/v0/voyage?format=csv&status=completed&date_from=2021-01-01&commodity=crude_oil&oil_products&departure_iso2=RU')

v1 <- v %>%
  filter(!is.na(destination_iso2s)) %>%
  rowwise() %>%
  mutate(destination_iso2s = list(jsonlite::fromJSON(gsub("'",'"', destination_iso2s))),
         destination_dates = list(jsonlite::fromJSON(gsub("'",'"', destination_dates)))
         ) %>%
  unnest(cols=c(destination_iso2s, destination_dates)) %>%
  select(id, status, departure_date_utc, arrival_date_utc, departure_port_area,
         correct_destination_iso2=destination_iso2,
         current_destination_iso2=destination_iso2s,
         current_destination_date=destination_dates) %>%
  mutate(is_correct = (current_destination_iso2==correct_destination_iso2),
         diff_date = as.integer(round(difftime(as.POSIXct(current_destination_date), as.POSIXct(departure_date_utc), units = "days"), 0))) %>%
  tidyr::complete(nesting(id, departure_date_utc, arrival_date_utc, departure_port_area), diff_date=seq(0, 365)) %>%
  arrange(id, diff_date) %>%
  group_by(id) %>%
  fill(is_correct, .direction = c('down')) %>%
  mutate(travel_duration = as.integer(difftime(arrival_date_utc, departure_date_utc, units='days')),
         is_correct = is_correct | (diff_date > travel_duration)) %>%
  group_by(departure_port_area, diff_date, is_correct) %>%
  summarise(count=n()) %>%
  tidyr::spread(is_correct, count, fill=0) %>%
  mutate(ratio=`TRUE`/(`TRUE`+`FALSE`)) %>%
  ungroup() %>%
  arrange(desc(diff_date))


ggplot(v1) +
  geom_line(aes(diff_date, ratio, col=departure_port_area)) +
  rcrea::theme_crea() +
  scale_y_continuous(breaks = seq(0,1,0.1),
                     labels=scales::percent,
                     name='Declared destination matching final destination',
                     expand = expansion(mult=c(0, 0)),
                     limits=c(0,1)) +
  scale_x_continuous(breaks = seq(0, 50, 5),
                     minor_breaks = seq(0,50,1),
                     limits=c(0,50),
                     expand = expansion(mult=c(0, 0)),
                     name='Days after departure') +
  theme(panel.grid.major.x = element_line('grey80'),
        panel.grid.minor.x = element_line('grey95')) +
  labs(color=NULL,
       subtitle='Declared destination matching final destination over time',
       caption='Crude oil and oil products only. Shipments in 2021 and 2022.')




