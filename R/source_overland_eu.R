#' Flows overland, except natural gas pipelined
#'
#' @return
#' @export
#'
#' @examples
overland_eu.get_flows <- function(){

  flows <- eurostat.get_overland_flows(split_in_days=F) %>%
    filter(destination_iso2 != 'EU') %>%
    filter(!grepl("gas|lng", commodity))

  flows <- utils.add_forecast(flows)

  flows <- flows %>%
    utils.split_month_in_days(value_cols=c('value_tonne')) %>%
    mutate(value_m3=NA_real_,
           value_mwh=NA_real_)

  # Russia’s Transneft says oil flows halted to the Czech Republic, Slovakia and Hungary over payment issue.
  # From august 4
  # flows[flows$departure_iso2 == 'RU' &
  #         flows$destination_iso2 %in% c('SK', 'HR', 'CZ') &
  #         flows$date >= '2022-08-04' &
  #         flows$commodity %in% c('pipeline_oil', 'oil_products_pipeline'), grepl('value_',names(flows))] = 0


  #HEURISTIC
  # Coal ban after August 10. Assuming 0 for overland coal
  flows[flows$departure_iso2 == 'RU' &
          flows$date >= '2022-08-10' &
          grepl('coal|coke', flows$commodity), grepl('value_',names(flows))] = 0


  #HEURISTIC
  # Germany stopping pipeline oil at the end of the year
  flows[flows$destination_iso2 == 'DE' &
          flows$date >= '2023-01-01' &
          grepl('oil', flows$commodity), grepl('value_',names(flows))] = 0

  #HEURISTIC
  # Stop oil products to EU after 2023-02-05
  flows[flows$date >= '2023-02-05' &
          grepl('oil_products', flows$commodity), grepl('value_',names(flows))] = 0

  #HEURISTIC
  # Russia halts oil supplies to Poland via Druzhba pipeline
  # https://english.alarabiya.net/business/energy/2023/02/25/Russia-halts-oil-supplies-to-Poland-via-he-Druzhba-pipeline
  flows[flows$date >= '2023-02-25' &
          flows$destination_iso2 %in% c('PL', 'HU', 'CZ', 'SK', 'DE') &
          grepl('pipeline_oil', flows$commodity), grepl('value_',names(flows))] = 0


  return(flows)
}

overland_eu.get_flows_old <- function(){

  trade_share <- utils.get_transport_share()
  #TODO missing coke

  flows_comtrade_eurostat = db.download_flows("comtrade_eurostat")
  # flows_comtrade_eurostat = comtrade_eurostat.get_flows(F)
  flows_eurostat_exeu = db.download_flows("eurostat_exeu")
  # flows_eurostat_exeu = eurostat_exeu.get_flows(F)

  flows_comtrade_eurostat_2022 = utils.expand_in_2022(flows_comtrade_eurostat, flows_eurostat_exeu)
  unique(flows_comtrade_eurostat_2022$commodity)

  flows <- flows_comtrade_eurostat_2022 %>%
    filter(partner=="Russia") %>%
    mutate(departure_iso2="RU") %>%
    mutate(commodity=recode(commodity, oil_others="oil_products")) %>%
    filter(country!="EU", EU) %>%
    filter(unit=="tonne") %>%
    left_join(trade_share %>%
                filter(transport != "pipeline" | !grepl("gas", commodity)) %>%
                filter(transport != "seaborne") %>%
                filter(transport != "other") %>%
                group_by(country, destination_iso2=iso2, commodity, transport) %>%
                summarise(share=sum(share, na.rm=T))) %>%
    mutate(value=value*share) %>%
    select(departure_iso2, commodity, transport, destination_iso2, date, value_tonne=value) %>%
    filter(!is.na(transport)) %>%
    mutate(commodity=paste(commodity, transport, sep="_")) %>%
    mutate(commodity = recode(commodity,
                              oil_pipeline="pipeline_oil",
                              oil_rail_road="crude_oil_rail_road")) %>%
    filter(value_tonne>0) %>%
    filter(!is.infinite(value_tonne)) %>%
    mutate(value_m3=NA_real_, value_mwh=NA_real_) %>%
    rename(month=date)

  # Split in days
  flows <- flows %>%
    left_join(
      tibble(date=seq(min(flows$month), max(flows$month) + lubridate::days_in_month(max(flows$month)) - 1, by="day")) %>%
        mutate(weight=1/lubridate::days_in_month(date),
               month=lubridate::floor_date(date, "month"))) %>%
    mutate(value_m3=value_m3*weight,
           value_tonne=value_tonne*weight
    ) %>%
    arrange(desc(date)) %>%
    select(-c(weight, month))


  # Russia’s Transneft says oil flows halted to the Czech Republic, Slovakia and Hungary over payment issue.
  # From august 4
  flows[flows$departure_iso2 == 'RU' &
          flows$destination_iso2 %in% c('SK', 'HR', 'CZ') &
          flows$date >= '2022-08-04' &
          flows$commodity %in% c('pipeline_oil', 'oil_products_pipeline'), grepl('value_',names(flows))] = 0


  # Coal ban after August 10. Assuming 0 for overland coal
  flows[flows$departure_iso2 == 'RU' &
          flows$date >= '2022-08-10' &
          grepl('coal|coke', flows$commodity), grepl('value_',names(flows))] = 0

  return(flows)
}

