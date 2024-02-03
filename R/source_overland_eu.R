#' Flows overland, except natural gas pipelined
#'
#' @return
#' @export
#'
#' @examples
overland_eu.get_flows <- function() {
  log_info("Get overland flows from Eurostat")
  flows <- eurostat.get_overland_flows(split_in_days = F) %>%
    filter(destination_iso2 != "EU") %>%
    filter(!grepl("gas|lng", commodity))

  log_info("Add forecast to overland flows")
  forecasted_flows <- utils.add_forecast(flows)

  log_info("Split months into days")
  forecasted_flows_by_day <- forecasted_flows %>%
    utils.split_month_in_days(value_cols = c("value_tonne")) %>%
    mutate(
      value_m3 = NA_real_,
      value_mwh = NA_real_
    )

  flows_with_heuristics <- forecasted_flows_by_day %>%
    rowwise() %>%
    mutate(
      value_tonne = case_when(
        # Coal ban after August 10. Assuming 0 for overland coal
        departure_iso2 == "RU" &
          date >= "2022-08-10" &
          grepl("coal|coke", commodity) ~ 0,
        # Germany stopping pipeline oil at the end of 2022
        destination_iso2 == "DE" &
          date >= "2023-01-01" &
          grepl("oil", commodity) ~ 0,
        # Stop oil products to EU after 2023-02-05
        date >= "2023-02-05" &
          grepl("oil_products", commodity) ~ 0,
        # Russia halts oil supplies to Poland via Druzhba pipeline
        departure_iso2 == "RU" &
          date >= "2023-02-25" &
          destination_iso2 %in% c("PL", "DE") &
          grepl("pipeline_oil", commodity) ~ 0,
        .default = value_tonne
      )
    ) %>%
    ungroup()

  return(
    flows_with_heuristics
  )
}
