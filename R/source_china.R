china.get_flows <- function(){
  #China pipeline oil imports 41 Mtpa
  #China pipeline gas 3.9 bcm in 2020; 5 bcm over 161 days in 2021-22 - assume 10 bcmpa
  #http://www.xinhuanet.com/english/2021-08/10/c_1310119621.htm
  #https://news.cgtn.com/news/2022-01-18/China-Russia-pipeline-delivers-15b-cubic-meters-of-natural-gas--16V6fC0jWQo/index.html

  # China is set to receive approximately 880,000 bpd of Russian oil via the two East Siberia-Pacific Ocean Pipelines (ESPO) and the Kazakhstan-China pipeline under government deals
  # https://www.reuters.com/business/energy/china-extends-record-imports-russian-oil-into-june-cuts-saudi-supply-trade-2022-07-06/

  tonne_per_bl = 0.136

  bind_rows(
    tibble(partner='China',
         commodity='natural_gas',
         value_m3=c(10e9/365),
         value_tonne=c(10e9/1000/365*0.7168),
         ) %>%
    tidyr::crossing(tibble(date=seq.Date(as.Date("2022-01-01"), lubridate::today(), by="day"))) %>%
    mutate(departure_iso2='RU',
           destination_iso2='CN'),



  tibble(
         value_m3=c(NA,NA,NA),
         value_tonne=c(41e6/365, 880e3*tonne_per_bl, 880e3*tonne_per_bl),
         date=c(as.Date("2022-01-01"), as.Date("2022-06-01"), lubridate::today())
  ) %>%
    full_join(tibble(date=seq.Date(as.Date("2022-01-01"), lubridate::today(), by="day"),
                     partner='China',
                     commodity=c('pipeline_oil'))) %>%
    arrange(date) %>%
    fill(value_tonne) %>%
    mutate(departure_iso2='RU',
           destination_iso2='CN')
  )
}
