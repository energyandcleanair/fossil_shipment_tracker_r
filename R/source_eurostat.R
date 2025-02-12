eurostat.get_flows <- function(use_cache = T) {
  commodity_codes <- list(
    "G3000" = "gas_all",
    "G3200" = "lng",
    "O4100_TOT" = "crude_oil",
    "O4600" = "oil_products",
    "2210" = "lignite",
    "2111" = "coal"
  )

  units <- list(
    "MIO_M3" = "m3",
    "TJ_GCV" = "TJ",
    "THS_T" = "tonne"
  )

  unit_factors <- list(
    "MIO_M3" = 1e6,
    "TJ_GCV" = 1,
    "THS_T" = 1000
  )

  f <- "cache/eurostat.RDS"
  if (use_cache && file.exists(f)) {
    return(readRDS(f))
  }

  gas_import <- eurostat::get_eurostat("nrg_ti_gasm", update_cache = T)
  gas_export <- eurostat::get_eurostat("nrg_te_gasm", update_cache = T) %>% mutate(values = values * -1)

  oil_import <- eurostat::get_eurostat("nrg_ti_oilm", update_cache = T)
  oil_export <- eurostat::get_eurostat("nrg_te_oilm", update_cache = T) %>% mutate(values = values * -1)

  coal_import <- eurostat::get_eurostat("nrg_122m", update_cache = T) %>% rename(siec = product)
  coal_export <- eurostat::get_eurostat("nrg_132m", update_cache = T) %>%
    rename(siec = product) %>%
    mutate(values = values * -1)

  data <- bind_rows(
    gas_import, gas_export,
    oil_import, oil_export,
    coal_import, coal_export
  ) %>%
    rename(
      value = values,
      date = time
    ) %>%
    filter(siec %in% names(commodity_codes)) %>%
    mutate(
      value = recode(unit, !!!unit_factors) * value,
      unit = recode(unit, !!!units),
      commodity = recode(siec, !!!commodity_codes)
    ) %>%
    filter(value > 0) %>%
    filter(!grepl("EU|EA19|EA", geo), partner != "TOTAL") %>%
    # filter(partner %in% c("RU","BY","UA")) %>%
    mutate(country = countrycode::countrycode(geo, "iso2c", "country.name",
      custom_match = c("UK" = "United Kingdom", "EL" = "Greece", "XK" = "XK")
    )) %>%
    mutate(partner = countrycode::countrycode(partner, "iso2c", "country.name")) %>%
    # mutate(value=values * 1e6 / MJ_per_kWh / 1000,
    #        unit="MWh/month") %>%
    group_by(country, unit, partner, date, commodity) %>%
    summarise(value = sum(value)) %>%
    mutate(source = "Eurostat") %>%
    ungroup()

  saveRDS(data, f)
  saveRDS(data, "inst/extdata/eurostat.RDS")
  return(data)
}

eurostat.get_overland_flows <- function(date_from = "2015-01-01", split_in_days = T) {
  log_level(REQUEST, "Fetching Eurostat overland flows from {date_from}")
  frequency <- "M"
  dest_countries <- paste(
    "EU", "AT", "BE", "BG", "CY", "CZ", "DE", "DK", "EE", "ES", "FI", "FR", "GR", "HR", "HU", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PL", "PT", "RO", "SE", "SI", "SK",
    sep = "+"
  )
  origin_countries <- paste(
    "RU",
    sep = "+"
  )
  commodities <- paste(
    "2701", "2704", "2705", "2706", "2709", "2710", "2711", "271111", "271121",
    sep = "+"
  )
  flow_directions <- paste(
    "1", # imports
    "2", # exports
    sep = "+"
  )
  transport_modes <- paste(
    "1", "2", "3", "4", "5", "7", "8", "9", "0",
    sep = "+"
  )
  indicator <- "QUANTITY_KG"

  date_from_formatted <- strftime(as.Date(date_from), "%Y-%m")
  date_to_formatted <- strftime(lubridate::today(), "%Y-%m")

  url <- glue(
    "https://ec.europa.eu/eurostat/api/comext/dissemination/sdmx/2.1/data/DS-059334/",
    "{frequency}.",
    "{dest_countries}.",
    "{origin_countries}.",
    "{commodities}.",
    "{flow_directions}.",
    "{transport_modes}.",
    "{indicator}/",
    "?format=SDMX-CSV",
    "&startPeriod={date_from_formatted}",
    "&endPeriod={date_to_formatted}",
    "&lang=en",
    "&label=both"
  )

  log_debug("Using URL {url}")

  flows_raw <- read_csv(url)

  log_level(DEBUG, "Overland flows has columns {paste(names(flows_raw), sep=',', collapse=',')}")

  to_code <- function(x) {
    sub("\\:.*", "", x)
  }
  to_label <- function(x) {
    sub(".*\\:", "", x)
  }
  flows <- flows_raw %>%
    rowwise() %>%
    mutate(
      value_tonne = ifelse(is.numeric(OBS_VALUE), OBS_VALUE, as.numeric(gsub(",", "", OBS_VALUE))) / 1e3
    ) %>%
    select(
      date = TIME_PERIOD,
      reporter,
      partner,
      commodity_code = product,
      flow,
      transport = transport_mode,
      unit = indicators,
      value_tonne
    ) %>%
    mutate_at(c("reporter", "partner", "commodity_code"), to_code) %>%
    mutate_at(c("flow", "transport"), to_label) %>%
    mutate(partner = ifelse(partner == "RU", "Russia", "World")) %>%
    filter(
      flow == "IMPORT",
      grepl("QUANTITY_KG", unit)
    ) %>%
    mutate(
      commodity = recode(commodity_code, !!!hs_commodities),
      transport = case_when(
        grepl("Fixed", transport) ~ "pipeline",
        grepl("Sea|water", transport) ~ "seaborne",
        grepl("Rail|Road", transport) ~ "rail_road",
        .default = "other"
      )
    ) %>%
    mutate(date = as.Date(paste0(date, "-01"))) %>%
    filter(
      grepl("^natural_gas|gas_all|^coal|oil|oil_products|lng|^coke", commodity),
      !is.na(value_tonne)
    ) %>%
    ungroup()


  flows <- flows %>%
    filter(partner == "Russia") %>%
    filter(transport %in% c("pipeline", "rail_road")) %>%
    mutate(departure_iso2 = "RU") %>%
    mutate(commodity = paste(commodity, transport, sep = "_")) %>%
    mutate(commodity = recode(commodity,
      oil_pipeline = "pipeline_oil",
      oil_rail_road = "crude_oil_rail_road"
    )) %>%
    rowwise() %>%
    filter(!is.na(value_tonne)) %>%
    ungroup() %>%
    group_by(departure_iso2, destination_iso2 = reporter, commodity, date) %>%
    summarise(
      value_tonne = sum(value_tonne)
    ) %>%
    ungroup()

  # Fill with zeros until last date
  flows <- flows %>%
    tidyr::complete(departure_iso2, destination_iso2, commodity, date,
      fill = list(value_tonne = 0)
    )

  if (split_in_days) {
    # Split in days
    flows <- flows %>%
      left_join(
        tibble(date = seq(min(flows$date), max(flows$date) + lubridate::days_in_month(max(flows$date)) - 1, by = "day")) %>%
          mutate(
            weight = 1 / lubridate::days_in_month(date),
            month = lubridate::floor_date(date, "month")
          )
      ) %>%
      mutate(value_tonne = value_tonne * weight) %>%
      arrange(desc(date)) %>%
      select(-c(weight, month))
  }

  return(flows)
}

eurostat.get_gas_flows <- function(use_cache = T) {
  gas_consumption <- eurostat::get_eurostat("nrg_cb_gasm", update_cache = !use_cache)
  gas_consumption %>%
    filter(
      unit == "MIO_M3",
      siec == "G3000"
    ) %>%
    mutate(type = recode(nrg_bal,
      IC_OBS = "consumption",
      IPRD = "production",
      IMP = "imports",
      EXP = "minus_exports",
      STK_CHG_MG = "storage_drawdown",
      .default = NA_character_,
    )) %>%
    filter(!is.na(type)) %>%
    mutate(values = ifelse(type %in% c("minus_exports", "storage_drawdown"), -values, values)) %>%
    select(iso2 = geo, date = time, value = values, type) %>%
    mutate(
      value_m3 = value * 1e6,
      unit = "m3",
      iso2 = recode(iso2, "UK" = "GB"),
      country = countrycode::countrycode(iso2, "iso2c", "country.name")
    ) %>%
    filter(!is.na(country)) %>%
    select(-c(value))
}
