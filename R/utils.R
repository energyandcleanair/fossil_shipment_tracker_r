utils.collect_comtrade <- function(partners, reporters, years, codes, frequency="monthly"){
  # Max 5 reporters at a time and one year

  if(partners=="World" & reporters=="all" & (frequency=="monthly")){
    start_dates <- lapply(years, function(y) paste0(y, c("-01","-06","-11"))) %>% unlist
    end_dates <- lapply(years, function(y) paste0(y, c("-05","-10","-12"))) %>% unlist
  }else{
    start_dates <- years
    end_dates <- years
  }

  lapply(seq_along(start_dates), function(i_date){
    message(start_dates[i_date], " - ", end_dates[i_date])
    lapply(split(reporters, rep(seq(1, length(reporters)/4), each=5)[1:length(reporters)]),
           function(reporters){
             print(reporters)
             res <- comtradr::ct_search(partners = partners,
                                        reporters = reporters,
                                        trade_direction = "all",
                                        commod_codes=codes,
                                        freq=frequency,
                                        start_date = start_dates[i_date],
                                        end_date = end_dates[i_date])
             if(nrow(res)==0){
               print("No row returned")
               stop("No row returned")
             }
             return(res)
           }) %>% do.call(bind_rows, .)
  }) %>% do.call(bind_rows, .)
}


utils.expand_in_2022 <- function(flows_comtrade_eurostat, flows_eurostat_exeu){

  # Project in the future
  # [Feb-Mar 2021 average] * (1 + [Nov-Dec 2021 YoY change])
  ratio_1 <- flows_comtrade_eurostat %>%
    ungroup() %>%
    mutate(year=lubridate::year(date)) %>%
    filter(year %in% c(2020, 2021),
           month(date) %in% c(11,12)) %>%
    group_by(year, country, partner, commodity, unit) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    tidyr::pivot_wider(names_from=year, values_from=value, names_prefix="value_") %>%
    mutate(ratio_1=value_2021/value_2020)


  # Seaborne reduction
  # since there is a reported 1/3 drop in seaborne cargoes
  # I would assume that that's 1/2 for Europe, and apply that reduction to all seaborne imports
  ratio_2 <- flows_comtrade_eurostat %>% ungroup() %>% distinct(country, partner, commodity, unit) %>%
    mutate(ratio_2=ifelse(grepl("lng", commodity), 0.5, 1))

  # Also need to reduce seaborne oil
  # Split oil between pipeline and sea
  ratio_3 <- flows_comtrade_eurostat %>% ungroup() %>% distinct(country, partner, commodity, unit) %>%
    left_join(
      flows_eurostat_exeu %>%
        filter(lubridate::year(date) %in% seq(2019,2021),
               commodity %in% c("oil","coal")) %>%
      group_by(commodity, transport, country, unit) %>%
      summarise(value=sum(value)) %>%
      group_by(country, unit, commodity) %>%
      mutate(share_sea=value/sum(value)) %>%
      arrange(country) %>%
      filter(transport=="sea") %>%
      mutate(ratio_3=1-(share_sea*0.5)) %>%
        select(-c(share_sea))) %>%
    mutate(ratio_3=tidyr::replace_na(ratio_3, 1))

  ratios <- ratio_1 %>% select(country, partner, commodity, unit, ratio_1) %>%
    left_join(ratio_2 %>% select(country, partner, commodity, unit, ratio_2)) %>%
    left_join(ratio_3 %>% select(country, partner, commodity, unit, ratio_3))

  flows_future <- flows_comtrade_eurostat %>%
    ungroup() %>%
    mutate(year=lubridate::year(date)) %>%
    filter(year %in% c(2021),
           month(date) %in% c(1, 2, 3)) %>%
    left_join(ratios) %>%
    mutate(date=date+lubridate::years(1),
           value=value*ratio_1*ratio_2*ratio_3)

  return(bind_rows(flows_comtrade_eurostat, flows_future) %>%
           filter(!is.na(date)))
}
