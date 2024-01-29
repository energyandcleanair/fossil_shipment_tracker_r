price.update_prices <- function(production = F, buffer_days = 60, rebuild = F) {
  prices <- price.get_prices(production = production)
  ok <- price.check_prices(prices)
  if (ok) {
    db.upload_prices_to_posgres(prices,
      production = production,
      buffer_days = buffer_days,
      rebuild = rebuild
    )
  } else {
    print("ERROR: prices not updated")
  }

  scenario_names <- price.get_scenario_names()
  db.upload_scenario_names(scenario_names, production = production)
}

price.get_prices <- function(production = F) {
  # This function gets all prices in one dataframe,
  # those 'default' ones, the ones specific to port,
  # and the capped one by ship_owner / insurer / destination

  # Get default values
  prices_daily_30day <- get_prices_daily(running_days = 30)
  predicted_prices <- price_models.get_predicted(
    production = production,
    add_urals_espo = T,
    prices_daily_30day = prices_daily_30day
  )
  # predicted_portprices <- price.get_predicted_portprices(production = production)
  # predicted_eurostat_prices <- price_models_eurostat.get_predicted_prices(production=production)

  p_default <- price_cap.apply_price_cap(
    predicted_prices = predicted_prices,
    cap_version = "default",
    scenario = "default"
  )

  # Create a version that imposes caps for all countries
  p_enhanced <- price_cap.apply_price_cap(
    predicted_prices = predicted_prices,
    destination_iso2s = NULL,
    add_ship_owner = F,
    add_ship_insurer = F,
    cap_version = "default",
    scenario = "enhanced"
  )

  # p_default <- price.get_capped_prices(
  #   predicted_prices=predicted_prices,
  #   predicted_port_prices=predicted_port_prices,
  #   predicted_eurostat_prices=predicted_eurostat_prices,
  #   prices_daily_30day=prices_daily_30day,
  #   production = production,
  #   scenario='default',
  #   version='default',
  # )

  all_prices <- bind_rows(
    p_default,
    p_enhanced
  ) %>%
    price.apply_china_ng_fix() %>%
    price.fill_old_values()

  return(all_prices)
}

price.fill_old_values <- function(p) {
  p %>%
    tidyr::complete(
      date = seq(as.Date("2015-01-01"), max(date(p$date)), by = "day"),
      scenario,
      commodity,
      fill = list(eur_per_tonne = NA)
    )
}

price.apply_china_ng_fix <- function(p) {
  # Specific one for China gas pipeline, directly from customs, so that total number matches
  china_ng_prices <- china.get_natural_gas_prices()

  p %>%
    left_join(
      china_ng_prices %>%
        select(date, eur_per_tonne_fix = eur_per_tonne) %>%
        mutate(
          commodity = "natural_gas",
          destination_iso2s = list("CN")
        )
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

price.get_predicted_portprices <- function(production = F) {
  # Ports
  ports <- read_csv("https://api.russiafossiltracker.com/v0/port?format=csv&iso2=RU", show_col_type = F)

  ports_ural <- ports %>%
    filter(iso2 == "RU", check_departure) %>%
    filter(lon < 40)

  ports_espo <- ports %>%
    filter(iso2 == "RU", check_departure) %>%
    filter(lon > 40)

  # Discounts
  brent <- russiacounter::get_brent()
  eur_per_usd <- price.eur_per_usd(date_from = min(date(brent$date)), date_to = min(max(date(brent$date)), lubridate::today()))
  tonne_per_bbl <- 0.138

  spread_ural <- russiacounter::get_ural_brent_spread() %>%
    select(date, add_brent = usd_per_bbl) %>%
    tidyr::complete(date = seq.Date(min(date), max(date), by = "day")) %>%
    fill(add_brent)

  price_ural <- brent %>%
    left_join(spread_ural) %>%
    arrange(desc(date)) %>%
    mutate(usd_per_bbl = brent + add_brent) %>%
    left_join(eur_per_usd) %>%
    mutate(eur_per_tonne = usd_per_bbl * eur_per_usd / tonne_per_bbl) %>%
    select(date, eur_per_tonne) %>%
    mutate(commodity = "crude_oil")

  price_espo <- get_espo(brent = brent, eur_per_usd = eur_per_usd) %>%
    mutate(commodity = "crude_oil")

  portprice_ural <- ports_ural %>%
    select(port_id = id) %>%
    crossing(price_ural) %>%
    filter(!is.na(eur_per_tonne))

  portprice_espo <- ports_espo %>%
    select(port_id = id) %>%
    crossing(price_espo) %>%
    filter(!is.na(eur_per_tonne))

  pp <- bind_rows(
    portprice_ural,
    portprice_espo
  ) %>%
    mutate(scenario = "default")

  # Nest country to match new price table structure
  pp_formatted <- pp %>%
    group_by(date, commodity, eur_per_tonne, scenario) %>%
    summarise(departure_port_ids = list(port_id)) %>%
    ungroup()

  return(pp_formatted)
}


price.get_capped_prices <- function(
    predicted_prices,
    predicted_portprices,
    predicted_eurostat_prices,
    production = F,
    scenario = "default",
    version = "default",
    prices_daily_30day = NULL,
    add_portprices = T) {
  caps <- price_cap.get_price_caps(p = predicted_prices, version = version)

  commodities <- read_csv("https://api.russiafossiltracker.com/v0/commodity?format=csv")
  seaborne_commodities <- commodities %>%
    filter(transport == "seaborne") %>%
    pull(id)
  eu <- setdiff(codelist$iso2c[!is.na(codelist$eu28)], "GB")
  g7 <- c("CA", "FR", "DE", "IT", "JP", "GB", "US", "UK")
  eu_g7 <- c(eu, g7)
  destination_iso2 <- eu_g7
  ship_owner_iso2s <- eu_g7
  ship_insurer_iso2s <- eu_g7
  date_start <- c("2022-12-06")

  # Create one version per destination_iso2, with no ship constraint
  pc_destination <- predicted_prices %>%
    left_join(caps, multiple = "all") %>%
    tidyr::unnest(destination_iso2s, keep_empty = T) %>%
    mutate(
      eur_per_tonne = case_when(
        (date >= date_start) & (destination_iso2s %in% destination_iso2) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm = T),
        T ~ eur_per_tonne
      ),
      # Not to have NULL nested with other countries
      destination_is_null = is.null(destination_iso2s) | is.na(destination_iso2s)
    ) %>%
    # To nest more together
    mutate(eur_per_tonne = round(eur_per_tonne, 3)) %>%
    group_by(across(-c(destination_iso2s, max_eur_per_tonne))) %>%
    summarise(across(destination_iso2s, list)) %>%
    select(-c(destination_is_null)) %>%
    ungroup()

  # Create a ship constraint (owner): regardless of destination and insurer
  pc_ship_owner <- pc_destination %>%
    filter(commodity %in% seaborne_commodities) %>%
    left_join(caps) %>%
    mutate(
      eur_per_tonne = case_when(
        (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm = T),
        T ~ eur_per_tonne
      ),
      ship_owner_iso2s = list(ship_owner_iso2s)
    )

  # Create a ship constraint (insurer): regardless of destination and owner
  pc_ship_insurer <- pc_destination %>%
    filter(commodity %in% seaborne_commodities) %>%
    left_join(caps) %>%
    mutate(
      eur_per_tonne = case_when(
        (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm = T),
        T ~ eur_per_tonne
      ),
      ship_insurer_iso2s = list(ship_insurer_iso2s)
    )

  # Port prices for destinations
  ppc_destination <- caps %>%
    filter(commodity %in% seaborne_commodities) %>%
    right_join(pp) %>%
    mutate(
      eur_per_tonne = case_when(
        (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm = T),
        T ~ eur_per_tonne
      ),
      destination_iso2s = list(destination_iso2)
    )

  # Port prices for ship_owner
  ppc_ship_owner <- caps %>%
    filter(commodity %in% seaborne_commodities) %>%
    right_join(pp) %>%
    mutate(
      eur_per_tonne = case_when(
        (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm = T),
        T ~ eur_per_tonne
      ),
      ship_owner_iso2s = list(ship_owner_iso2s)
    )

  # Port prices for ship_insurer
  ppc_ship_insurer <- caps %>%
    filter(commodity %in% seaborne_commodities) %>%
    right_join(pp) %>%
    mutate(
      eur_per_tonne = case_when(
        (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm = T),
        T ~ eur_per_tonne
      ),
      ship_insurer_iso2s = list(ship_insurer_iso2s)
    )


  # Create a deditacted Ural and Espo price
  # which is used by Kpler
  p_espo <- get_espo() %>%
    mutate(
      commodity = "crude_oil_espo",
      scenario = !!scenario
    ) %>%
    filter(!is.na(eur_per_tonne)) %>%
    fill_gaps_and_future()

  p_urals <- get_urals() %>%
    mutate(
      commodity = "crude_oil_urals",
      scenario = !!scenario
    ) %>%
    filter(!is.na(eur_per_tonne)) %>%
    fill_gaps_and_future()

  p_urals_espo <- bind_rows(p_urals, p_espo)

  # Create one version per destination_iso2, with no ship constraint
  pc_urals_espo_destination <- caps %>%
    right_join(p_urals_espo %>% mutate(destination_iso2s = list(c(eu_g7)))) %>%
    tidyr::unnest(destination_iso2s, keep_empty = T) %>%
    mutate(
      eur_per_tonne = case_when(
        (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm = T),
        T ~ eur_per_tonne
      ),
      # Not to have NULL nested with other countries
      destination_is_null = is.null(destination_iso2s) | is.na(destination_iso2s)
    ) %>%
    # To nest more together
    mutate(eur_per_tonne = round(eur_per_tonne, 3)) %>%
    group_by(across(-c(destination_iso2s, max_eur_per_tonne))) %>%
    summarise(across(destination_iso2s, list)) %>%
    select(-c(destination_is_null)) %>%
    ungroup()

  # Create a ship constraint (insurer): regardless of destination and owner
  pc_urals_espo_ship_insurer <-
    bind_rows(
      p_urals_espo,
      pc_urals_espo_destination
    ) %>%
    filter(commodity %in% seaborne_commodities) %>%
    left_join(caps) %>%
    mutate(
      eur_per_tonne = case_when(
        (date >= date_start) ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm = T),
        T ~ eur_per_tonne
      ),
      ship_insurer_iso2s = list(ship_insurer_iso2s)
    )


  pc <- bind_rows(
    pc_destination,
    pc_ship_owner,
    pc_ship_insurer,
    p_urals_espo,
    pc_urals_espo_destination,
    pc_urals_espo_ship_insurer
  )

  if (add_portprices) {
    pc <- bind_rows(
      pc,
      pp,
      ppc_ship_owner,
      ppc_ship_insurer,
      ppc_destination,
    )
  }

  pc <- pc %>%
    ungroup() %>%
    select(-c(max_eur_per_tonne)) %>%
    mutate(scenario = !!scenario)

  return(pc)
}


price.get_capped_portprices <- function(production = F) {
  p <- price.get_predicted_portprices(production = production)

  # Cap using 2021H1 prices, weighted average, one globally
  caps <- readRDS(system.file("extdata", "comtrade_eurostat.RDS", package = "russiacounter")) %>%
    filter(lubridate::floor_date(date, "halfyear") == "2021-01-01") %>%
    group_by(commodity, unit) %>%
    summarise_at("value", sum, na.rm = T) %>%
    tidyr::spread(unit, value) %>%
    mutate(eur_per_tonne = eur / tonne) %>%
    select(-c(eur, tonne)) %>%
    mutate(commodity = recode(commodity,
      oil = "crude_oil"
    ))

  caps <- bind_rows(caps, caps %>%
    filter(commodity == "crude_oil") %>%
    mutate(commodity = "pipeline_oil"))

  eur_per_usd <- price.eur_per_usd(
    date_from = min(p$date),
    date_to = min(max(p$date), lubridate::today())
  )

  eur_per_usd_2021H1 <- eur_per_usd %>%
    filter(lubridate::floor_date(date, "halfyear") == "2021-01-01") %>%
    summarise(eur_per_usd_base = mean(eur_per_usd))

  eur_per_usd %>%
    tidyr::crossing(eur_per_usd_2021H1) %>%
    mutate(price_adjustment = eur_per_usd / eur_per_usd_base) %>%
    select(date, price_adjustment) %>%
    tidyr::crossing(caps) %>%
    mutate(eur_per_tonne = case_when(
      commodity %in% c("natural_gas", "lng") ~ eur_per_tonne,
      T ~ eur_per_tonne * price_adjustment
    )) %>%
    select(-c(price_adjustment)) %>%
    rename(max_eur_per_tonne = eur_per_tonne) %>%
    ungroup() %>%
    right_join(p) %>%
    mutate(
      eur_per_tonne = case_when(
        (date >= "2022-07-01") ~ pmin(eur_per_tonne, max_eur_per_tonne, na.rm = T),
        T ~ eur_per_tonne
      ),
      scenario = "pricecap"
    ) %>%
    select(-c(max_eur_per_tonne))
}


price.check_prices <- function(p) {
  ok <- !any(is.na(p$eur_per_tonne) & (p$date >= "2020-01-01"))
  ok <- ok & !any(is.na(p$scenario))
  ok <- ok & (min(p$eur_per_tonne, na.rm = T) >= 0)
  ok <- ok & all(c("destination_iso2s", "ship_owner_iso2s", "ship_insurer_iso2s", "date", "commodity", "eur_per_tonne", "scenario") %in% names(p))
  ok <- ok & nrow(p) > 0
  return(ok)
}
