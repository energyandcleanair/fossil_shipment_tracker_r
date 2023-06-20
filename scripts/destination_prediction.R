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
         is_correct = is_correct | (diff_date > travel_duration))

v2 <- v1 %>%
  group_by(departure_port_area, diff_date, is_correct) %>%
  summarise(count=n()) %>%
  tidyr::spread(is_correct, count, fill=0) %>%
  mutate(ratio=`TRUE`/(`TRUE`+`FALSE`)) %>%
  ungroup() %>%
  arrange(desc(diff_date))


ggplot(v2) +
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


# Investigate weird ones --------------------------------------------------

first_destination <- v1 %>%
  group_by(id) %>%
  filter(! current_destination_iso2 %in% c('RU','None'),
         !is.na(current_destination_iso2)) %>%
  summarize(first_destination_iso2=head(current_destination_iso2, 1)) %>%
  ungroup()


v1 %>%
  filter(!is.na(correct_destination_iso2)) %>%
  ungroup() %>%
  left_join(first_destination) %>%
  filter(correct_destination_iso2=='RU',
         first_destination_iso2!='RU') %>%
  distinct(id, .keep_all = T)

# Prediction --------------------------------------------------------------
library(randomForest)
library(mlbench)
library(caret)
library(e1071)
library(caTools)

library(caret)
set.seed(998)

first_destination <- v1 %>%
  group_by(id) %>%
  filter(! current_destination_iso2 %in% c('RU','None'),
         !is.na(current_destination_iso2)) %>%
  summarize(first_destination_iso2=head(current_destination_iso2, 1)) %>%
  ungroup()

data <- v1 %>%
  filter(!is.na(correct_destination_iso2)) %>%
  ungroup() %>%
  left_join(first_destination) %>%
  group_by(id) %>%
  arrange(current_destination_date) %>%
  mutate(previous_destination_iso2=lag(current_destination_iso2)) %>%
  ungroup() %>%
  select(departure_port_area,
         correct_destination_iso2,
         current_destination_iso2,
         first_destination_iso2,
         diff_date,
         travel_duration) %>%
  filter(diff_date < travel_duration) %>%
  select(-c(travel_duration))


data <- data[complete.cases(data),]

inTraining <- createDataPartition(data$correct_destination_iso2, p = .75, list = FALSE)
training <- data[ inTraining,]
testing  <- data[-inTraining,]


control <- trainControl(method='repeatedcv',
                        number=5,
                        repeats=1)

fit <- train(correct_destination_iso2 ~ ., data = training,
             method = "ranger",
             trControl = control)
saveRDS(fit, 'fit.RDS')
fit <- readRDS('fit.RDS')
print(fit)


t2 <- testing %>% filter(current_destination_iso2 %in% training$current_destination_iso2)
t2$predicted <- predict(fit, t2)

t3 <- t2 %>%
  mutate(model=predicted==correct_destination_iso2,
         raw=current_destination_iso2==correct_destination_iso2) %>%
  tidyr::gather("source", "is_correct", model, raw) %>%
  group_by(departure_port_area, diff_date, source, is_correct) %>%
  summarise(count=n()) %>%
  tidyr::spread(is_correct, count, fill=0) %>%
  mutate(ratio=`TRUE`/(`TRUE`+`FALSE`)) %>%
  ungroup() %>%
  arrange(desc(diff_date))

ggplot(t3) +
  geom_line(aes(diff_date, ratio, col=departure_port_area, linetype=source)) +
  rcrea::theme_crea() +
  scale_y_continuous(breaks = seq(0,1,0.1),
                     labels=scales::percent,
                     name='Declared destination matching final destination',
                     expand = expansion(mult=c(0, 0)),
                     # limits=c(0,1)
  ) +
  scale_x_continuous(breaks = seq(0, 50, 5),
                     minor_breaks = seq(0,50,1),
                     limits=c(0,50),
                     expand = expansion(mult=c(0, 0)),
                     name='Days after departure') +
  theme(panel.grid.major.x = element_line('grey80'),
        panel.grid.minor.x = element_line('grey95')) +
  labs(color=NULL,
       subtitle='Declared destination matching final destination over time',
       caption='Crude oil and oil products only. Shipments in 2021 and 2022.') +
  facet_wrap(~departure_port_area)



View(table(as.character(t2$predicted),
           as.character(t2$correct_destination_iso2)))

predict(fit, testing, type = "prob")


