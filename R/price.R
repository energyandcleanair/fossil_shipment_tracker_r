price.update_prices <- function(production = F, buffer_days = 60, rebuild = F) {
  log_info("Fetch get all prices")
  prices <- price.get_prices(production = production)
  log_info("Validate prices")
  ok <- price.check_prices(prices)

  if (ok) {
    log_info("Uploading prices to database")
    db.upload_prices_to_posgres(prices,
      production = production,
      buffer_days = buffer_days,
      rebuild = rebuild
    )
  } else {
    log_warn("Prices are not valid, not uploading to database")
  }

  log_info("Get scenario names")
  scenario_names <- price.get_scenario_names()
  log_info("Uploading scenario names to database")
  db.upload_scenario_names(scenario_names, production = production)

  if (!ok) {
    stop("Prices were not valid")
  }
}

price.get_prices <- function(production = F) {
  # This function gets all prices in one dataframe,
  # those 'default' ones, the ones specific to port,
  # and the capped one by ship_owner / insurer / destination

  # Get default values
  log_info("Get actual prices")
  prices_daily_30day <- get_prices_daily(running_days = 30)
  log_info("Get predicted prices")
  predicted_prices <- price_models.get_predicted(
    production = production,
    add_urals_espo = T,
    prices_daily_30day = prices_daily_30day
  )

  log_info("Create model: current scenario")
  p_default <- price_cap.apply_price_cap(
    predicted_prices = predicted_prices,
    cap_version = "default",
    scenario = "default"
  )

  log_info("Create model: impose price caps on all countries")
  # Create a version that imposes caps for all countries
  p_enhanced <- price_cap.apply_price_cap(
    predicted_prices = predicted_prices,
    destination_iso2s = NULL,
    add_ship_owner = F,
    add_ship_insurer = F,
    cap_version = "default",
    scenario = "enhanced"
  )

  log_info("Combine all prices together")
  all_prices <- bind_rows(
    p_default,
    p_enhanced
  ) %>%
    # Filter all_prices to not go beyond 14 days after today, all sources must
    # provide at least this but some go beyond it
    filter(date <= today() + 14)

  log_info("Fix all prices")
  fixed <- all_prices %>%
    price.apply_china_ng_fix() %>%
    price.fill_old_values()

  return(fixed)
}

price.fill_old_values <- function(p) {
  log_info("Fill old values")

  p %>%
    tidyr::complete(
      date = seq(as.Date("2015-01-01"), max(date(p$date)), by = "day"),
      scenario,
      commodity,
      fill = list(eur_per_tonne = NA)
    )
}

price.apply_china_ng_fix <- function(prices) {
  log_info("Apply China NG fix")

  # Specific one for China gas pipeline, directly from customs, so that total number matches
  china_ng_prices <- china.get_natural_gas_prices()

  prices %>%
    left_join(
      china_ng_prices %>%
        select(date, eur_per_tonne_fix = eur_per_tonne) %>%
        mutate(
          commodity = "natural_gas",
          destination_iso2s = list("CN")
        ),
      by = join_by(date, commodity, destination_iso2s)
    ) %>%
    mutate(eur_per_tonne = coalesce(eur_per_tonne_fix, eur_per_tonne)) %>%
    select(-c(eur_per_tonne_fix))
}

price.get_scenario_names <- function() {
  tibble(
    id = c("default", "2021H1", "usd20", "usd30", "usd40"),
    name = c("USD60/bbl (actual)", "2021H1 prices", "USD20/bbl", "USD30/bbl", "USD40/bbl")
  )
}

price.check_prices <- function(prices) {
  missing_columns <- setdiff(
    c(
      "destination_iso2s",
      "ship_owner_iso2s",
      "ship_insurer_iso2s",
      "date",
      "commodity",
      "eur_per_tonne",
      "scenario"
    ),
    names(prices)
  )

  bad_prices <- prices %>%
    filter(
      (is.na(eur_per_tonne) & (date >= "2020-01-01")) |
        (eur_per_tonne < 0)
    )

  missing_scenario <- any(is.na(prices$scenario))

  no_rows <- nrow(prices) == 0

  ok <- nrow(bad_prices) == 0 & length(missing_columns) == 0 & !no_rows & !missing_scenario

  # Print out any of the issues
  if (nrow(bad_prices) > 0) {
    log_warn("Expected all prices to be good but some were bad:")
    print(bad_prices %>% group_by(scenario, commodity) %>% summarise(n = n()))
    # Take a sample and print it out, if there's more than 10
    if (nrow(bad_prices) > 10) {
      print(bad_prices %>% sample_n(10))
    } else {
      print(bad_prices)
    }
  }
  if (length(missing_columns) > 0) {
    log_warn(glue("Expected to find columns but were missing: {missing_columns}"))
  }
  if (no_rows) {
    log_warn("No rows found")
  }
  if (missing_scenario) {
    log_warn("Some entries were missing a scenario")
  }

  return(ok)
}
