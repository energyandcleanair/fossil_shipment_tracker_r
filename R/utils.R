utils.read_csv <- function(url, ...) {
  start_time <- Sys.time()
  res <- read_csv(url, ..., guess_max = 1e6)
  end_time <- Sys.time()
  log_info("Took {end_time - start_time} for {url}")
  return(res)
}

utils.recode_comtrade_commodity <- function(df) {
  df$commodity[grep("crude$", df$commodity)] <- "crude_oil"
  df$commodity[grep("not crude", df$commodity)] <- "oil_products"
  df$commodity[grep("^Coal.*ovoids", df$commodity)] <- "coal"
  df$commodity[grep("liquefied", df$commodity)] <- "lng"
  df$commodity[grep("Coal gas", df$commodity)] <- "coal_gas"
  df$commodity[grep("gases", df$commodity)] <- "natural_gas"
  return(df)
}

utils.collect_comtrade <- function(partners, reporters, years, codes, frequency = "monthly", trade_flow = "all", stop_if_no_row = T) {
  readRenviron(".Renviron")
  comtradr::set_primary_comtrade_key(Sys.getenv("COMTRADE_PRIMARY_KEY"))

  # Max 5 reporters at a time and one year
  if (all(partners == "World") & all(reporters == "all") & (frequency == "monthly")) {
    start_dates <- lapply(years, function(y) paste0(y, c("-01", "-06", "-11"))) %>% unlist()
    end_dates <- lapply(years, function(y) paste0(y, c("-05", "-10", "-12"))) %>% unlist()
  } else {
    start_dates <- paste0(years, "-01")
    end_dates <- paste0(years, "-12")
  }

  frequency <- list(monthly = "M", annualy = "A")[[frequency]]
  partners_iso3 <- countrycode::countrycode(partners, "country.name", "iso3c", custom_match = c(World = "World", all = "all"))
  reporters_iso3 <- countrycode::countrycode(reporters, "country.name", "iso3c", custom_match = c(all = "all", "EUR" = "EUR"))

  log_info("Collecting data from comtrade for reporters: {paste(reporters, collapse=', ')}; partners: {paste(partners, collapse=', ')}; years: {paste(years, collapse=', ')}; codes: {paste(codes, collapse=', ')}")

  pbapply::pblapply(seq_along(start_dates), function(i_date) {
    pbapply::pblapply(reporters_iso3, function(reporter_iso3) {
      pbapply::pblapply(codes, function(code) {
        log_info("Collecting data for {reporter_iso3} {code} {start_dates[i_date]}-{end_dates[i_date]}")
        res <- tibble()
        itry <- 0
        ntries <- 3
        while (nrow(res) == 0 & (itry < ntries)) {
          res <- tryCatch(
            {
              comtradr::ct_get_data(
                partner = partners_iso3,
                reporter = reporter_iso3,
                flow_direction = trade_flow,
                commodity_code = code,
                freq = frequency,
                start_date = start_dates[i_date],
                end_date = end_dates[i_date]
              )
            },
            error = function(error) {
              return(tibble())
            }
          )

          if (nrow(res) == 0 & (itry < ntries)) {
            itry <- itry + 1
            log_debug("No row returned. Trying again")
          }
        }

        if (nrow(res) == 0 & stop_if_no_row) {
          log_warn("No row returned.")
        }

        return(res)
      }) %>%
        do.call(bind_rows, .)
      # Filter(function(x){nrow(x)>0}, .)
    }) %>%
      do.call(bind_rows, .)
  }) %>%
    # Filter(function(x){nrow(x)>0}, .) %>%
    do.call(bind_rows, .)
}

utils.get_transport_share <- function() {
  # f <- "inst/extdata/DS-1262527_1_Data_for_transport_share_20220424.RDS"
  f <- system.file("extdata", "DS-1262527_1_Data_for_transport_share_20220424.RDS",
    package = "russiacounter"
  )
  log_info("Reading {f}")

  if (!file.exists(f)) {
    stop(sprintf("Can't find file %s", f))
  }


  trade <- readRDS(f) %>%
    mutate(partner = ifelse(PARTNER == "RU", "Russia", "World")) %>%
    mutate(year = as.integer(PERIOD / 100)) %>%
    mutate(Value = gsub(",| ", "", Value) %>% as.numeric(), ) %>%
    filter(
      FLOW == "IMPORT",
      INDICATORS == "QUANTITY_IN_TONS"
    ) %>%
    select(year,
      iso2 = REPORTER, partner, commodity_code = PRODUCT,
      transport = TRANSPORT_MODE, value = Value
    ) %>%
    group_by(year, iso2, partner, commodity_code, transport) %>%
    summarise(value_tonne = sum(value, na.rm = T)) %>%
    mutate(country = countrycode::countrycode(iso2, "iso2c", "country.name")) %>%
    mutate(value_tonne = tidyr::replace_na(value_tonne, 0))


  trade <- trade %>%
    filter(commodity_code %in% names(hs_commodities)) %>%
    mutate(commodity = recode(commodity_code, !!!hs_commodities))

  # Clean commodities
  trade$transport[grep("Fixed", trade$transport)] <- "pipeline"
  trade$transport[grep("Sea|water", trade$transport)] <- "seaborne"
  trade$transport[grep("Rail|Road", trade$transport)] <- "rail_road"

  # We ignore others (#TODO investigate a bit further Inland Waterway)
  trade$transport[!grepl("pipeline|seaborne|rail_road", trade$transport)] <- "other"

  trade <- trade %>%
    filter(
      grepl("^natural_gas|gas_all|^coal|oil|oil_products|lng|^coke", commodity),
      !is.na(value_tonne)
    ) %>%
    ungroup()


  # Check visually
  trade %>%
    group_by(year, country, commodity, partner) %>%
    mutate(share = value_tonne / sum(value_tonne)) %>%
    ggplot() +
    geom_bar(
      stat = "identity",
      aes(share, paste(commodity, partner), fill = transport)
    ) +
    facet_wrap(~country)


  # Select Russia when available,
  # world otherwise
  trade_share <- trade %>%
    filter(value_tonne > 0) %>%
    ungroup() %>%
    distinct(year, country, commodity, partner) %>%
    group_by(year, country, commodity) %>%
    arrange(partner) %>% # Russia < World
    dplyr::slice_head(n = 1) %>%
    filter(commodity %in% c("coal", "lng", "coke", "oil", "oil_products", "natural_gas", "gas_all")) %>%
    left_join(
      trade %>%
        group_by(year, country, commodity, partner) %>%
        mutate(share = value_tonne / sum(value_tonne))
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
utils.expand_overland_in_2022 <- function(flows_eurostat_exeu) {
  # Project in the future
  # [Feb-Mar 2021 average] * (1 + [Nov-Dec 2021 YoY change])
  ratio_1 <- flows_eurostat_exeu %>%
    ungroup() %>%
    mutate(year = lubridate::year(date)) %>%
    filter(
      year %in% c(2020, 2021),
      month(date) %in% c(11, 12)
    ) %>%
    group_by(year, country, partner, commodity, transport, unit) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
    mutate(ratio_1 = value_2021 / value_2020)

  ratios <- ratio_1 %>% select(country, partner, commodity, transport, unit, ratio_1)

  flows_future <- flows_eurostat_exeu %>%
    filter(transport %in% c("pipeline", "Rail", "Road")) %>%
    ungroup() %>%
    mutate(year = lubridate::year(date)) %>%
    filter(
      year %in% c(2021),
      month(date) %in% c(1, 2, 3, 4, 5)
    ) %>%
    left_join(ratios) %>%
    mutate(
      date = date + lubridate::years(1),
      value = value * ratio_1
    ) %>%
    select(-c(ratio_1)) %>%
    filter(!is.na(value), !is.infinite(value)) %>%
    filter(date >= "2022-01-01")


  # Remove pipeline gas
  flows_future <- flows_future %>%
    filter(country != "EU") %>%
    # Remove pipeline gas
    filter(transport != "pipeline" | (!grepl("gas", commodity))) %>%
    # Remove aggregated
    # Add iso2s
    mutate(destination_iso2 = countrycode(country, "country.name", "iso2c")) %>%
    mutate(departure_iso2 = countrycode(partner, "country.name", "iso2c")) %>%
    # Regroup commodities
    mutate(commodity = recode(commodity,
      oil = "crude_oil",
      oil_others = "oil_products",
      gas_all = "gas"
    )) %>%
    mutate(transport = recode(transport,
      Rail = "rail_road",
      Road = "rail_road"
    )) %>%
    group_by(departure_iso2, destination_iso2, date, commodity, transport, unit) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    filter(unit %in% c("tonne")) %>%
    tidyr::pivot_wider(names_from = "unit", values_from = "value", names_prefix = "value_") %>%
    # Spread across days
    rename(month = date) %>%
    left_join(
      tibble(date = seq(min(flows_future$date), max(flows_future$date) + lubridate::days(31), by = "day")) %>%
        mutate(
          weight = 1 / lubridate::days_in_month(date),
          month = lubridate::floor_date(date, "month")
        )
    ) %>%
    mutate(value_tonne = value_tonne * weight) %>%
    filter(date >= "2021-12-01") %>%
    select(-c(month, weight)) %>%
    ungroup() %>%
    mutate(commodity = paste(commodity, transport, sep = "_")) %>%
    mutate(commodity = recode(commodity,
      crude_oil_pipeline = "pipeline_oil"
    )) %>%
    select(-c(transport)) %>%
    mutate(value_m3 = 0, value_mwh = 0)

  return(flows_future)
}


utils.expand_in_2022 <- function(flows_comtrade_eurostat, flows_eurostat_exeu) {
  # Project in the future
  # [Feb-Mar 2021 average] * (1 + [Nov-Dec 2021 YoY change])
  ratio_1 <- flows_comtrade_eurostat %>%
    ungroup() %>%
    mutate(year = lubridate::year(date)) %>%
    filter(
      year %in% c(2020, 2021),
      month(date) %in% c(11, 12)
    ) %>%
    group_by(year, country, partner, commodity, unit) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    tidyr::pivot_wider(names_from = year, values_from = value, names_prefix = "value_") %>%
    mutate(ratio_1 = value_2021 / value_2020)


  # Seaborne reduction
  # crude oil: -6%, LNG -47%, oil products -11%
  ratio_2 <- flows_comtrade_eurostat %>%
    ungroup() %>%
    distinct(country, partner, commodity, unit) %>%
    mutate(ratio_2 = ifelse(grepl("lng", commodity), 0.53, 1))

  # Also need to reduce seaborne oil
  # Split oil between pipeline and sea
  ratio_3 <- flows_comtrade_eurostat %>%
    ungroup() %>%
    distinct(country, partner, commodity, unit) %>%
    left_join(
      flows_eurostat_exeu %>%
        filter(
          lubridate::year(date) %in% seq(2019, 2021),
          commodity %in% c("oil", "oil_others", "coal")
        ) %>%
        group_by(commodity, transport, country, unit) %>%
        summarise(value = sum(value)) %>%
        group_by(country, unit, commodity) %>%
        mutate(share_sea = value / sum(value)) %>%
        arrange(country) %>%
        filter(transport == "sea") %>%
        mutate(sea_reduction = recode(commodity,
          oil_others = 0.11,
          oil = 0.06,
          coal = 0.5
        )) %>%
        mutate(ratio_3 = 1 - (share_sea * sea_reduction)) %>%
        select(-c(share_sea, sea_reduction))
    ) %>%
    mutate(ratio_3 = tidyr::replace_na(ratio_3, 1))

  ratios <- ratio_1 %>%
    select(country, partner, commodity, unit, ratio_1) %>%
    left_join(ratio_2 %>% select(country, partner, commodity, unit, ratio_2)) %>%
    left_join(ratio_3 %>% select(country, partner, commodity, unit, ratio_3))

  flows_future <- flows_comtrade_eurostat %>%
    ungroup() %>%
    mutate(year = lubridate::year(date)) %>%
    filter(
      year %in% c(2021),
      # month(date) %in% c(1, 2, 3, 4, 5, 6, 7, 8, )
    ) %>%
    left_join(ratios) %>%
    mutate(
      date = date + lubridate::years(1),
      value = value * ratio_1 * ratio_2 * ratio_3
    ) %>%
    select(-c(ratio_1, ratio_2, ratio_3))

  return(bind_rows(flows_comtrade_eurostat, flows_future) %>%
    filter(!is.na(date)))
}


utils.add_forecast <- function(flows) {
  predict <- function(df, keys) {
    df <- df %>%
      arrange(date) %>%
      filter(date >= max(date) - lubridate::years(3)) %>%
      tidyr::complete(
        date = seq.Date(as.Date(min(df$date)),
          as.Date(max(df$date)),
          by = "month"
        ),
        fill = list(value_tonne = 0)
      )

    dfts <- ts(df$value_tonne,
      frequency = 12,
      start = c(
        lubridate::year(min(df$date)),
        lubridate::month(min(df$date))
      )
    )

    end_date <- lubridate::today() + lubridate::days(30)
    n_months <- lubridate::interval(max(df$date), end_date) %/% months(1)
    # components_dfts <- decompose(dfts)
    # plot(components_dfts)
    tryCatch(
      {
        model <- HoltWinters(dfts)
        predicted <- forecast(model, h = n_months)

        predicted %>%
          sweep::sw_sweep(.) %>%
          filter(key == "forecast") %>%
          mutate(date = as.Date(paste("01", index), "%d %b %Y")) %>%
          select(date, value_tonne = value) %>%
          # Cap at latest observed value! Very conservative
          mutate(value_tonne = pmax(pmin(value_tonne, tail(df$value_tonne, 1)), 0))
      },
      error = function(error) {
        log_warn("Failed to forecast")
        # If failed, we just assume constant value
        return(tibble(
          date = seq.Date(max(df$date) + months(1), end_date, by = "month"),
          value_tonne = tail(df$value_tonne, 1)
        ))
      }
    )
  }

  forecasted <- flows %>%
    group_by(departure_iso2, destination_iso2, commodity) %>%
    group_modify(predict)

  bind_rows(
    flows %>% mutate(type = "observed"),
    forecasted %>% mutate(type = "forecasted")
  )
}

utils.split_month_in_days <- function(df, value_cols) {
  df %>%
    rename(month = date) %>%
    left_join(
      tibble(date = seq(min(df$date), max(df$date) + lubridate::days_in_month(max(df$date)) - 1, by = "day")) %>%
        mutate(
          weight = 1 / lubridate::days_in_month(date),
          month = lubridate::floor_date(date, "month")
        )
    ) %>%
    mutate_at(value_cols, function(x) {
      x * .$weight
    }) %>%
    arrange(desc(date)) %>%
    select(-c(weight, month))
}

utils.get_eu_iso2s <- function() {
  codelist$iso2c[!is.na(codelist$eu28) & codelist$eu28 != "NA"]
}
