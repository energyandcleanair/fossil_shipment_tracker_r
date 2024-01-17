entsog_new.get_flows <- function(date_from = "2021-11-01", date_to = NULL, use_cache = T) {
  if (is.null(date_to)) {
    date_to <- as.character(lubridate::today() + lubridate::days(1))
  }

  years <- seq(lubridate::year(date_from), lubridate::year(date_to))

  flows <- lapply(years, function(year) {
    date_from_ <- max(as.Date(date_from), as.Date(paste0(year, "-01-01")))
    date_to_ <- min(as.Date(date_to), as.Date(paste0(year, "-12-31")))
    read_csv(sprintf(
      "https://api.russiafossiltracker.com/v0/entsogflow?type=crossborder,production&format=csv&date_from=%s&date_to=%s",
      date_from_, date_to_
    ))
  }) %>%
    bind_rows()

  flows <- flows %>% filter(destination_iso2 != "RU")

  flows_formatted <- flows %>%
    select(
      from_country = commodity_origin_iso2,
      to_country = commodity_destination_iso2,
      date,
      value = value_m3
    )

  flows_sourced <- process_iterative(flows_formatted)

  sum(flows_sourced$value[flows_sourced$from_country == "RU"], na.rm = T) / 1e9
  sum(flows_formatted$value[flows_formatted$from_country == "RU"]) / 1e9
  sum(flows_sourced$value[flows_sourced$from_country == flows_sourced$to_country], na.rm = T) / 1e9 == 0

  # Add production
  flows_sourced <- bind_rows(
    flows_sourced %>%
      filter(from_country != to_country),
    flows_formatted %>%
      filter(from_country == to_country)
  )


  flows_sourced <- flows_sourced %>%
    mutate(value = round(value, 2)) %>% # Some values are ~-1e-15
    rename(value_m3 = value) %>%
    mutate(value_tonne = value_m3 * kg_per_m3 / 1000) %>%
    mutate(value_mwh = value_m3 * gcv_kWh_per_m3 / 1000) %>%
    mutate(commodity = "natural_gas") %>%
    rename(
      departure_iso2 = from_country,
      destination_iso2 = to_country
    )

  return(flows_sourced)
}
