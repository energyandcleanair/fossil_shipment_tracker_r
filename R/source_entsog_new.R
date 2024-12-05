entsog_new._replace_lng_with_origin <- function(overland_flows, ..., date_from, date_to) {
  # We want to replace entries that say origin "LNG" with the origin of the LNG
  # instead.
  #
  # We will do this by, for each entry point that is LNG, attributing the value
  # proportionally based on the origin of each of the imports to that entry point.
  # We will get the proportion from the seaborne imports and also removing the
  # transhipments.
  #
  # The overland_flows is a tibble with columns including:
  # - commodity_origin_iso2
  # - commodity_destination_iso2
  # - value_m3
  #
  # If the commodity_origin_iso2 is "lng" then the commodity_destination_iso2
  # is the entry point for that LNG (where it was imported to).

  # Get the countries that are LNG importers
  lng_importers <- overland_flows %>%
    filter(commodity_origin_iso2 == "lng") %>%
    pull(commodity_destination_iso2) %>%
    unique()

  lng_csv_arg_for_api <- paste(lng_importers, collapse = ",")

  api_key <- Sys.getenv("RUSSIA_FOSSIL_TRACKER_API_KEY")

  years <- seq(lubridate::year(date_from), lubridate::year(date_to))

  rolling_period <- 30

  # Get seaborne imports of LNG to the countries excluding transhipments, chunked by year.
  seaborne_imports <- lapply(years, function(year) {
    # As we're doing a rolling period, we need to request the data from the
    # rolling period. We add an extra day because the API is inclusive of the
    # date_from and date_to.
    date_from_ <- max(as.Date(date_from), as.Date(paste0(year, "-01-01"))) - lubridate::days(rolling_period) + 1
    date_to_ <- min(as.Date(date_to), as.Date(paste0(year, "-12-31")))
    log_info(glue("Getting seaborne imports of LNG to EU lng ports from {date_from_} to {date_to_}"))
    read_csv(
      glue(
        "https://api.russiafossiltracker.com/v1/kpler_trade",
        "?api_key={api_key}",
        "&format=csv",
        "&commodity=lng",
        "&destination_date_from={date_from_}",
        "&destination_date_to={date_to_}",
        "&destination_iso2={lng_csv_arg_for_api}",
        "&exclude_sts=true",
        "&aggregate_by=destination_iso2,origin_iso2,destination_date,",
        "&rolling_days={rolling_period}",
        "&check_complete=false"
      )
    )
  }) %>%
    bind_rows()

  # We want the proportion of the value of the LNG imports by origin at each
  # destination for each day.
  seaborne_imports_proportion <- seaborne_imports %>%
    tidyr::drop_na() %>%
    group_by(destination_iso2, origin_iso2, destination_date) %>%
    summarise(value_m3 = sum(value_m3)) %>%
    ungroup() %>%
    group_by(destination_iso2, destination_date) %>%
    mutate(proportion = value_m3 / sum(value_m3)) %>%
    ungroup() %>%
    # Where proportion is NA, fill from the recent valeus
    group_by(origin_iso2, destination_date) %>%
    tidyr::fill(proportion) %>%
    ungroup() %>%
    select(
      destination_iso2, origin_iso2, destination_date, proportion
    )

  # We want to replace the row for each LNG import in the overland_flows with
  # the proportion of the value of the LNG imports by origin at the destination
  # for that day.
  lng_overland_flows <-
    lng_overland_attributed <- overland_flows %>%
    filter(commodity_origin_iso2 == "lng") %>%
    left_join(
      seaborne_imports_proportion,
      by = c(
        "commodity_destination_iso2" = "destination_iso2",
        "date" = "destination_date"
      )
    ) %>%
    mutate(
      value_m3 = value_m3 * proportion,
      commodity_origin_iso2 = origin_iso2,
      from_method = "lng",
    ) %>%
    # select only original columns from overland_flows
    select(
      overland_flows %>% names()
    )

  other_overland_flows <- overland_flows %>%
    filter(commodity_origin_iso2 != "lng") %>%
    select(
      overland_flows %>% names()
    )

  recombined_flows <- bind_rows(
    other_overland_flows,
    lng_overland_attributed
  ) %>% arrange(
    date, commodity_origin_iso2, commodity_destination_iso2
  )

  percentage_diff_tolerance <- 0.01
  # Assert that the sum of the values of the recombined flows is the same as the
  # sum of the values of the original flows.
  recombined_sum <- sum(recombined_flows$value_m3, na.rm = T)
  original_sum <- sum(overland_flows$value_m3, na.rm = T)
  percentage_diff <- abs(recombined_sum - original_sum) / ((recombined_sum + original_sum) / 2)
  if (percentage_diff > percentage_diff_tolerance) {
    stop(glue("The sum of the values of the recombined flows ({recombined_sum}) is not the same as the sum of the values of the original flows ({original_sum})."))
  }

  return(recombined_flows)
}

entsog_new.get_flows <- function(date_from = "2021-11-01", date_to = NULL, use_cache = T) {
  if (is.null(date_to)) {
    date_to <- as.character(lubridate::today() + lubridate::days(1))
  }

  years <- seq(lubridate::year(date_from), lubridate::year(date_to))

  log_info("Getting flows from the ENTSOG endpoint in our API")

  flows_raw <- lapply(years, function(year) {
    date_from_ <- max(as.Date(date_from), as.Date(paste0(year, "-01-01")))
    date_to_ <- min(as.Date(date_to), as.Date(paste0(year, "-12-31")))
    read_csv(sprintf(
      "https://api.russiafossiltracker.com/v0/entsogflow?type=crossborder,production&format=csv&date_from=%s&date_to=%s",
      date_from_, date_to_
    ))
  }) %>%
    bind_rows()

  log_info("Turning flows into format suitable for the model")
  flows_formatted <- flows_raw %>%
    filter(destination_iso2 != "RU") %>%
    # Set all sources to pipeline, this will be overwritten where appropriate
    # by entsog_new._replace_lng_with_origin.
    mutate(
      from_method = "pipeline",
      to_method = "pipeline"
    ) %>%
    entsog_new._replace_lng_with_origin(date_from = date_from, date_to = date_to) %>%
    select(
      from_country = commodity_origin_iso2,
      from_method = from_method,
      to_country = commodity_destination_iso2,
      to_method = to_method,
      date,
      value = value_m3
    )

  log_info("Processing flows")
  flows_sourced <- process_iterative(flows_formatted)

  sum(flows_sourced$value[flows_sourced$from_country == "RU"], na.rm = T) / 1e9
  sum(flows_formatted$value[flows_formatted$from_country == "RU"]) / 1e9
  sum(flows_sourced$value[flows_sourced$from_country == flows_sourced$to_country], na.rm = T) / 1e9 == 0

  # Add production
  log_info("Combine production with processed flows")
  flows_sourced <- bind_rows(
    flows_sourced %>%
      filter(from_country != to_country),
    flows_formatted %>%
      filter(from_country == to_country)
  )

  log_info("Converting to table format")

  flows_sourced <- flows_sourced %>%
    mutate(value = round(value, 2)) %>% # Some values are ~-1e-15
    rename(value_m3 = value) %>%
    mutate(value_tonne = value_m3 * kg_per_m3 / 1000) %>%
    mutate(value_mwh = value_m3 * gcv_kWh_per_m3 / 1000) %>%
    mutate(commodity = "natural_gas") %>%
    rename(
      departure_iso2 = from_country,
      destination_iso2 = to_country,
      entry_mode = from_method,
    ) %>%
    # Exclude to_method lng
    filter(to_method != "lng") %>%
    select(
      departure_iso2,
      destination_iso2,
      entry_mode,
      date,
      value_m3,
      value_tonne,
      value_mwh,
      commodity
    )

  return(flows_sourced)
}
