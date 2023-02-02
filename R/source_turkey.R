turkey.get_flows <- function(){


  eurostat_codes <- c("nrg_ti_gasm", "nrg_te_gasm")
  exim <- eurostat_codes %>% lapply(function(code) eurostat::get_eurostat(id=code))
  names(exim) <- eurostat_codes


  # Share of pipeline: 77%
  # https://www.mees.com/2022/3/4/economics-finance/turkey-2021-gas-imports-ukraine-war-puts-russia-reliance-in-focus/dd8176c0-9bcd-11ec-96b1-3f07b501dca9#:~:text=*Turkey%20imported%20a%20record%2060.1,in%20the%20form%20of%20LNG.


  flows <- exim %>%
    bind_rows(.id='code') %>%
    filter(geo=="TR") %>%
    filter(partner=="RU") %>%
    filter(unit=="MIO_M3") %>%
    filter(lubridate::year(time) %in% seq(2018,2023)) %>%
    filter(values != 0) %>%
    tidyr::spread(siec, values) %>%
    mutate(GPIPELINE=G3000-coalesce(G3200, 0)) %>%
    mutate(
      date=time,
      value_m3 = GPIPELINE * 1e6,
      value_tonne = GPIPELINE * kg_per_m3/1000)

  flows <- flows %>%
    select(date, value_m3, value_tonne) %>%
    rename(month=date)

  # Project in future
  m_o_m <- flows %>%
    mutate(imonth = lubridate::month(month),
           year = lubridate::year(month)) %>%
    arrange(month) %>%
    mutate(prev_value_tonne=lag(value_tonne)) %>%
    mutate(month_on_month = (value_tonne-prev_value_tonne)/prev_value_tonne) %>%
    group_by(imonth) %>%
    summarise(m_o_m=mean(month_on_month, na.rm=T))

  # Project in the future using seasonality
  while(max(flows$month) < lubridate::floor_date(lubridate::today() + 7, "month")){
    flows <- bind_rows(flows,
                       flows %>%
              filter(month==max(flows$month)) %>%
              mutate(month = month %m+% months(1),
                     imonth = lubridate::month(month)) %>%
              left_join(m_o_m) %>%
              mutate(value_m3 = value_m3 * (1 + m_o_m),
                     value_tonne = value_tonne * (1 + m_o_m)) %>%
              select(-c(imonth, m_o_m)))
  }


  # Split in days
  flows <- flows %>%
    left_join(
      tibble(date=seq(min(flows$month), max(flows$month) + 31, by="day")) %>%
        mutate(weight=1/lubridate::days_in_month(date),
               month=lubridate::floor_date(date, "month"))) %>%
    mutate(value_m3=value_m3*weight,
           value_tonne=value_tonne*weight
           ) %>%
    mutate(departure_iso2='RU',
           destination_iso2='TR',
           commodity="natural_gas") %>%
    arrange(desc(date)) %>%
    select(-c(weight))

  return(flows)
}
