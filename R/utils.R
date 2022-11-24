utils.read_csv <- function(url, ...){
  start_time <- Sys.time()
  res <- read_csv(url, ..., guess_max=1e6)
  end_time <- Sys.time()
  print(sprintf("Took %s for %s", end_time-start_time, url))
  return(res)
}

utils.recode_comtrade_commodity <- function(df){
  df$commodity[grep('crude$', df$commodity)] <- "crude_oil"
  df$commodity[grep('not crude', df$commodity)] <- "oil_products"
  df$commodity[grep('^Coal.*ovoids', df$commodity)] <- "coal"
  df$commodity[grep('liquefied', df$commodity)] <- "lng"
  df$commodity[grep('Coal gas', df$commodity)] <- "coal_gas"
  df$commodity[grep('gases', df$commodity)] <- "natural_gas"
  return(df)
}

utils.collect_comtrade <- function(partners, reporters, years, codes, frequency="monthly", trade_flow='all', stop_if_no_row=T){
  # Max 5 reporters at a time and one year

  if(all(partners=="World") & all(reporters=="all") & (frequency=="monthly")){
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
             res <- tibble()
             itry <- 0
             ntries <- 3
             while(nrow(res)==0 & (itry<ntries)){
               res <- comtradr::ct_search(partners = partners,
                                          reporters = reporters,
                                          trade_direction = trade_flow,
                                          commod_codes=codes,
                                          freq=frequency,
                                          start_date = start_dates[i_date],
                                          end_date = end_dates[i_date])

               Sys.sleep(5)
               if(nrow(res)==0 & (itry<ntries)){
                 itry <- itry + 1
                 print("No row returned. Trying again")
               }
             }
             if(nrow(res)==0 & stop_if_no_row){
               warning("No row returned.")
             }
             return(res)
           }) %>%
      Filter(function(x){nrow(x)>0}, .) %>%
      do.call(bind_rows, .)
  }) %>%
    Filter(function(x){nrow(x)>0}, .) %>%
    do.call(bind_rows, .)
}

#' Distribution of Russian pipelined gas amongst countries
#'
#' @return
#' @export
#'
#' @examples
utils.get_russia_pipeline_distribution <- function(){

  # comtrade_eurostat.get_flows(use_cache=T) %>%
  #   filter(lubridate::year(date)==2021,
  #          unit=="tonne",
  #          grepl("Russia", partner),
  #          commodity=="natural_gas",
  #          EU) %>%
  #   group_by(country) %>%
  #   summarise(value=sum(value, na.rm=T)) %>%
  #   mutate(share=value/sum(value)) %>%
  #   arrange(desc(share))

  # bp_share <- readxl::read_xlsx(f,
  #                   sheet="Gas - Trade movts - pipeline",
  #                   range="A3:W41") %>%
  #   tidyr::pivot_longer(cols = -To,
  #                       names_to="partner") %>%
  #   rename(country=To) %>%
  #   mutate(commodity="natural_gas",
  #          unit="m3",
  #          value=value*1e9) %>%
  #   filter(!country %in% c("North America", "S. & Cent. America", "Europe",
  #                          "CIS","Middle East","Middle East", "Asia Pacific")) %>%
  #   mutate(commodity="natural_gas") %>%
  #   filter(partner=="Russian Federation") %>%
  #   filter(country %in% c("Belgium",
  #                         "France",
  #                         "Germany",
  #                         "Italy",
  #                         "Netherlands",
  #                         "Spain",
  #                         # "Turkey",
  #                         # "Ukraine",
  #                         "United Kingdom",
  #                         "Other EU",
  #                         "Rest of Europe")) %>%
  #   mutate(iso2 = countrycode(country, "country.name", "iso2c"))
  #
  # # Need to fill other EU
  # f_eurostat <- "cache/eurostatdata.RDS"
  # if(!file.exists(f_eurostat)){
  #   eurostat_codes <- c("nrg_ti_gasm", "nrg_te_gasm")
  #   exim <- eurostat_codes %>% lapply(eurostat::get_eurostat)
  #   names(exim) <- eurostat_codes
  #
  #   eurostat_data <- exim %>% bind_rows(.id='code') %>%
  #     filter(lubridate::year(time) %in% seq(2016,2021)) %>% filter(values != 0)
  #   saveRDS(eurostat_data, f_eurostat)
  # }else{
  #   eurostat_data <- readRDS(f_eurostat)
  # }
  #
  #
  # eurostat_share_othereu <- eurostat_data %>%
  #   filter(!grepl('EU|EA19', geo), code=='nrg_ti_gasm', unit=='MIO_M3',
  #          partner %in% c('RU', 'TOTAL')) %>%
  #   # WE DO THIS ONLY FOR REST OF EU
  #   mutate(geo = recode(geo, EL="GR", UK="GB")) %>%
  #   # filter(! geo %in% bp_share$iso2) %>%
  #   select(siec, partner, unit, geo, time, values) %>%
  #   mutate(year=lubridate::year(time)) %>%
  #   filter(year %in% seq(2019, 2021)) %>%
  #   group_by(partner, unit, geo, siec) %>%
  #   summarise(values=sum(values, na.rm=T)) %>%
  #   tidyr::spread(siec, values) %>%
  #   # Pipeline = Total- LNG
  #   mutate(GPIPELINE=G3000-tidyr::replace_na(G3200,0)) %>%
  #   tidyr::gather("siec","values",c("G3000","G3200","GPIPELINE")) %>%
  #
  #   # Impose share from Russia to Austria and Hungary
  #   tidyr::spread(partner, values)  %>%
  #   mutate(commodity=recode(siec, G3000="gas_all", G3200="lng", GPIPELINE="natural_gas")) %>%
  #   left_join(
  #     tibble(geo=c('AT','HR'), forced_share_from_russia=c(.8,.7)) %>% tidyr::crossing(commodity=c("gas_all","natural_gas"))
  #   ) %>%
  #   mutate(RU = ifelse(!is.na(forced_share_from_russia), TOTAL * forced_share_from_russia, RU)) %>%
  #   select(-c(forced_share_from_russia)) %>%
  #   tidyr::gather("partner","values",c("RU","TOTAL")) %>%
  #   filter(partner=="RU") %>%
  #   filter(! geo %in% c("EA","TR")) %>% # We don't consider Turkey pipeline
  #   ungroup() %>%
  #   select(-c(siec)) %>%
  #   tidyr::spread(commodity, values) %>%
  #   mutate(share_of_russia = natural_gas / sum(natural_gas, na.rm=T)) %>%
  #   select(destination_iso2=geo, share_of_russia) %>%
  #   mutate(commodity="natural_gas") %>%
  #   arrange(desc(share_of_russia))
  #
  #
  #
  # russia_export_distrib <-
  #   bind_rows(bp_share %>% filter(country != "Other EU"),
  #             bp_share %>%
  #               filter(country == "Other EU") %>%
  #               select(-c(country, iso2)) %>%
  #               tidyr::crossing(eurostat_share_othereu %>% select(iso2=destination_iso2, share_of_russia)) %>%
  #               mutate(value = tidyr::replace_na(value * share_of_russia, 0)) %>%
  #               mutate(country=countrycode(iso2, "iso2c", "country.name")) %>%
  #               select(-c(share_of_russia))) %>%
  # mutate(share_of_russia = value / sum(value, na.rm=T)) %>%
  #   select(iso2, share_of_russia) %>%
  #   filter(!is.na(iso2))
  #
  # return(russia_export_distrib)
}


utils.get_transport_share <- function(){

  #f <- "inst/extdata/DS-1262527_1_Data_for_transport_share_20220424.RDS"
  f <- system.file("extdata", "DS-1262527_1_Data_for_transport_share_20220424.RDS",
                    package="russiacounter")
  print(sprintf("Reading %s",f))

  if(!file.exists(f)){
    stop(sprintf("Can't find file %s",f))
  }

  trade <- readRDS(f) %>%
    mutate(partner=ifelse(PARTNER=="RU", "Russia", "World")) %>%
    mutate(year=as.integer(PERIOD/100)) %>%
    mutate(Value = gsub(',| ', '', Value) %>% as.numeric,) %>%
    filter(FLOW=="IMPORT",
           INDICATORS=="QUANTITY_IN_TONS") %>%
    select(year, iso2=REPORTER, partner, commodity_code=PRODUCT,
           transport=TRANSPORT_MODE, value=Value) %>%
    group_by(year, iso2, partner, commodity_code, transport) %>%
    summarise(value_tonne=sum(value, na.rm=T)) %>%
    mutate(country=countrycode::countrycode(iso2, "iso2c", "country.name")) %>%
    mutate(value_tonne=tidyr::replace_na(value_tonne, 0))


  trade <- trade %>%
    filter(commodity_code %in% names(hs_commodities)) %>%
    mutate(commodity=recode(commodity_code, !!!hs_commodities))

  # trade %>% ungroup() %>%
  #   filter(value>0, unit=="tonne") %>%
  #   group_by(country, commodity, unit) %>% summarise(value=sum(value)) %>%
  #   tidyr::spread(commodity, value, fill = 0) %>%
  #   mutate(gas_all2=lng+natural_gas) %>%
  #   select(country, unit, gas_all, gas_all2)

  # Some countries only share total gas numbers
  # trade %>% ungroup() %>% filter(value>0, unit=="tonne") %>%
  #   group_by(country, commodity, transport, unit) %>% summarise(value=sum(value)) %>%
  #   tidyr::spread(commodity, value, fill = 0) %>%
  #   mutate(gas_all2=lng+natural_gas) %>%
  #   select(country, unit, transport, gas_all, gas_all2) %>% View()

  # Clean commodities
  trade$transport[grep('Fixed', trade$transport)] <- "pipeline"
  trade$transport[grep('Sea|water', trade$transport)] <- "seaborne"
  trade$transport[grep('Rail|Road', trade$transport)] <- "rail_road"

  # We ignore others (#TODO investigate a bit further Inland Waterway)
  trade$transport[!grepl('pipeline|seaborne|rail_road', trade$transport)] <- "other"

  trade <- trade %>%
    filter(grepl('^natural_gas|gas_all|^coal|oil|oil_products|lng|^coke', commodity),
           !is.na(value_tonne)) %>%
    ungroup()


  # Check visually
  trade %>%
    group_by(year, country, commodity, partner) %>%
    mutate(share=value_tonne / sum(value_tonne)) %>%
    ggplot() +
    geom_bar(stat="identity",
             aes(share, paste(commodity, partner), fill=transport)) +
    facet_wrap(~country)


  # Select Russia when available,
  # world otherwise
  trade_share <- trade %>%
    filter(value_tonne>0) %>%
    ungroup() %>%
    distinct(year, country, commodity, partner) %>%
    group_by(year, country, commodity) %>%
    arrange(partner) %>% # Russia < World
    dplyr::slice_head(n=1) %>%
    filter(commodity %in% c("coal", "lng", "coke", "oil", "oil_products", "natural_gas", "gas_all")) %>%
    left_join(
      trade %>%
        group_by(year, country, commodity, partner) %>%
        mutate(share=value_tonne / sum(value_tonne))
    )

  return(trade_share %>%
           ungroup() %>%
           select(country, iso2, transport, commodity, share))
}

#' Overland daily flows in 2022 (except for natural gas that is covered by ENTSOG)
#'
#' @param flows_eurostat_exeu
#'
#' @return
#' @export
#'
#' @examples
utils.expand_overland_in_2022 <- function(flows_eurostat_exeu){

  # Project in the future
  # [Feb-Mar 2021 average] * (1 + [Nov-Dec 2021 YoY change])
  ratio_1 <- flows_eurostat_exeu %>%
    ungroup() %>%
    mutate(year=lubridate::year(date)) %>%
    filter(year %in% c(2020, 2021),
           month(date) %in% c(11,12)) %>%
    group_by(year, country, partner, commodity, transport, unit) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    tidyr::pivot_wider(names_from=year, values_from=value, names_prefix="value_") %>%
    mutate(ratio_1=value_2021/value_2020)

  ratios <- ratio_1 %>% select(country, partner, commodity, transport, unit, ratio_1)

  flows_future <- flows_eurostat_exeu %>%
    filter(transport %in% c("pipeline", "Rail", "Road")) %>%
    ungroup() %>%
    mutate(year=lubridate::year(date)) %>%
    filter(year %in% c(2021),
           month(date) %in% c(1, 2, 3, 4, 5)) %>%
    left_join(ratios) %>%
    mutate(date=date+lubridate::years(1),
           value=value*ratio_1) %>%
    select(-c(ratio_1)) %>%
    filter(!is.na(value), !is.infinite(value)) %>%
    filter(date >= '2022-01-01')


  # Remove pipeline gas
  flows_future <- flows_future %>%
    filter(country != "EU") %>%
    # Remove pipeline gas
    filter(transport != "pipeline" | (!grepl("gas", commodity))) %>%
    # Remove aggregated
    # Add iso2s
    mutate(destination_iso2=countrycode(country, "country.name", "iso2c")) %>%
    mutate(departure_iso2=countrycode(partner, "country.name", "iso2c")) %>%
    # Regroup commodities
    mutate(commodity = recode(commodity,
                              oil="crude_oil",
                              oil_others="oil_products",
                              gas_all="gas")) %>%
    mutate(transport = recode(transport,
                              Rail="rail_road",
                              Road="rail_road")) %>%
    group_by(departure_iso2, destination_iso2, date, commodity, transport, unit) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    filter(unit %in% c("tonne")) %>%
    tidyr::pivot_wider(names_from="unit", values_from="value", names_prefix="value_") %>%
    # Spread across days
    rename(month=date) %>%
    left_join(
      tibble(date=seq(min(flows_future$date), max(flows_future$date) + lubridate::days(31), by="day")) %>%
        mutate(weight=1/lubridate::days_in_month(date),
               month=lubridate::floor_date(date, "month"))) %>%
    mutate(value_tonne=value_tonne*weight) %>%
    filter(date>="2021-12-01") %>%
    select(-c(month, weight)) %>%
    ungroup() %>%
    mutate(commodity = paste(commodity, transport, sep="_")) %>%
    mutate(commodity = recode(commodity,
                              crude_oil_pipeline="pipeline_oil")) %>%
    select(-c(transport)) %>%
    mutate(value_m3=0, value_mwh=0)

  return(flows_future)
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
  # crude oil: -6%, LNG -47%, oil products -11%
  ratio_2 <- flows_comtrade_eurostat %>% ungroup() %>% distinct(country, partner, commodity, unit) %>%
    mutate(ratio_2=ifelse(grepl("lng", commodity), 0.53, 1))

  # Also need to reduce seaborne oil
  # Split oil between pipeline and sea
  ratio_3 <- flows_comtrade_eurostat %>%
    ungroup() %>%
    distinct(country, partner, commodity, unit) %>%
    left_join(
      flows_eurostat_exeu %>%
        filter(lubridate::year(date) %in% seq(2019,2021),
               commodity %in% c("oil","oil_others","coal")) %>%
      group_by(commodity, transport, country, unit) %>%
      summarise(value=sum(value)) %>%
      group_by(country, unit, commodity) %>%
      mutate(share_sea=value/sum(value)) %>%
      arrange(country) %>%
      filter(transport=="sea") %>%
      mutate(sea_reduction=recode(commodity,
                              oil_others=0.11,
                              oil=0.06,
                              coal=0.5)) %>%
      mutate(ratio_3=1-(share_sea*sea_reduction)) %>%
        select(-c(share_sea, sea_reduction))) %>%
    mutate(ratio_3=tidyr::replace_na(ratio_3, 1))

  ratios <- ratio_1 %>% select(country, partner, commodity, unit, ratio_1) %>%
    left_join(ratio_2 %>% select(country, partner, commodity, unit, ratio_2)) %>%
    left_join(ratio_3 %>% select(country, partner, commodity, unit, ratio_3))

  flows_future <- flows_comtrade_eurostat %>%
    ungroup() %>%
    mutate(year=lubridate::year(date)) %>%
    filter(year %in% c(2021),
           # month(date) %in% c(1, 2, 3, 4, 5, 6, 7, 8, )
           ) %>%
    left_join(ratios) %>%
    mutate(date=date+lubridate::years(1),
           value=value*ratio_1*ratio_2*ratio_3) %>%
    select(-c(ratio_1, ratio_2, ratio_3))

  return(bind_rows(flows_comtrade_eurostat, flows_future) %>%
           filter(!is.na(date)))
}
