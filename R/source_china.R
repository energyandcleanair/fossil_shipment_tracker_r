china.get_flows <- function(){
  #China pipeline oil imports 41 Mtpa
  #China pipeline gas 3.9 bcm in 2020; 5 bcm over 161 days in 2021-22 - assume 10 bcmpa
  #http://www.xinhuanet.com/english/2021-08/10/c_1310119621.htm
  #https://news.cgtn.com/news/2022-01-18/China-Russia-pipeline-delivers-15b-cubic-meters-of-natural-gas--16V6fC0jWQo/index.html
  tibble(partner='China',
         commodity=c('pipeline_oil', 'natural_gas'),
         value_m3=c(NA, 10e9/365),
         value_tonne=c(41e6/365, 10e9/1000/365*0.7168),
         ) %>%
    tidyr::crossing(tibble(date=seq.Date(as.Date("2022-01-01"), lubridate::today(), by="day"))) %>%
    mutate(departure_iso2='RU',
           destination_iso2='CN')
}
