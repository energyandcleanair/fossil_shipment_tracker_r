#' Flows overland, except natural gas pipelined
#'
#' @return
#' @export
#'
#' @examples
overland_eu.get_flows <- function(){

  trade_share <- utils.get_transport_share()
  #TODO missing coke

  flows_comtrade_eurostat = db.download_flows("comtrade_eurostat")
  flows_eurostat_exeu = db.download_flows("eurostat_exeu")
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
    mutate(value_m3=NA, value_mwh=NA) %>%
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

  return(flows)
}
