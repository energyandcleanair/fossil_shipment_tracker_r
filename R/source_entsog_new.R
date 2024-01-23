entsog_new.get_flows <- function(date_from = "2021-11-01", date_to = NULL, use_cache = T) {
  if (is.null(date_to)) {
    date_to <- as.character(lubridate::today() + lubridate::days(1))
  }

  flows <- entsog_new.get_flows_for_period(date_from, date_to)
  flows_ma <- entsog_new.get_flows_for_period_with_ma(date_from, date_to, 90)

  message(names(flows))
  message(names(flows_ma))

  flows_with_ma <- flows %>%
    left_join(
      flows_ma,
      by = c("from_country", "to_country", "date")
    ) %>%
    select(
      from_country,
      to_country,
      date,
      value,
      value_ma
    )

  flows_sourced <- process_iterative(flows_with_ma)

  message(entsog_new.print_stats(flows_with_ma, flows_sourced))

  # Add production
  flows_sourced <- bind_rows(
    flows_sourced %>%
      filter(from_country != to_country),
    flows_with_ma %>%
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

entsog_new.get_flows_for_period_with_ma <- function(date_from, date_to, moving_average_period) {
  flows <- entsog_new.get_flows_for_period(date_from - lubridate::days(moving_average_period), date_to)

  flows_ma <- flows %>%
    group_by(from_country, to_country) %>%
    arrange(date) %>%
    mutate(
      value_ma = zoo::rollmean(value, moving_average_period, fill = 0, align = "right")
    ) %>%
    ungroup() %>%
    select(
      !value
    )

  return(flows_ma)
}

entsog_new.get_flows_for_period <- function(date_from, date_to) {
  years <- seq(lubridate::year(date_from), lubridate::year(date_to))

  flows <- lapply(years, function(year) {
    entsog_new.get_flows_for_year(year, date_from, date_to)
  }) %>%
    bind_rows()

  return(
    flows %>%
      filter(destination_iso2 != "RU") %>%
      entsog_new.format_flows()
  )
}

entsog_new.get_flows_for_year <- function(year, date_from, date_to) {
  date_from_ <- max(as.Date(date_from), as.Date(paste0(year, "-01-01")))
  date_to_ <- min(as.Date(date_to), as.Date(paste0(year, "-12-31")))
  return(
    read_csv(
      sprintf(
        "https://api.russiafossiltracker.com/v0/entsogflow?type=crossborder,production&format=csv&date_from=%s&date_to=%s",
        date_from_, date_to_
      ),
      show_col_types = F
    )
  )
}

entsog_new.format_flows <- function(flows) {
  return(
    flows %>%
      select(
        from_country = commodity_origin_iso2,
        to_country = commodity_destination_iso2,
        date,
        value = value_m3
      )
  )
}

entsog_new.print_stats <- function(flows, flows_sourced) {
  sum(flows_sourced$value[flows_sourced$from_country == "RU"], na.rm = T) / 1e9
  sum(flows$value[flows$from_country == "RU"]) / 1e9
  sum(flows_sourced$value[flows_sourced$from_country == flows_sourced$to_country], na.rm = T) / 1e9 == 0
}
