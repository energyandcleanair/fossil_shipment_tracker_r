fill_past <- function(result, date_from) {
  if (nrow(result) == 0) {
    stop("Cannot fill past on empty data. Input dataframe has 0 rows.")
  }
  result %>%
    ungroup() %>%
    tidyr::complete(date = seq(as.Date(date_from), lubridate::date(min(date)), by = "day")) %>%
    arrange(desc(date)) %>%
    tidyr::fill(setdiff(names(.), "date"), .direction = "down")
}

fill_gaps_and_future <- function(result) {
  if (nrow(result) == 0) {
    stop("Cannot fill gaps on empty data. Input dataframe has 0 rows.")
  }
  result %>%
    ungroup() %>%
    tidyr::complete(date = seq(lubridate::date(min(date)), max(lubridate::date(max(date)), Sys.Date()) + lubridate::days(14), by = "day")) %>%
    arrange(desc(date)) %>%
    tidyr::fill(setdiff(names(.), "date"), .direction = "up")
}

force_utc <- function(df) {
  df %>% mutate(date = force_tz(date, "UTC"))
}

market_source_table_name <- function(source_name) {
  if (!grepl("^[a-z0-9]+(_[a-z0-9]+)*$", source_name)) {
    stop(
      glue::glue(
        "source_name must already be a valid market source table suffix using lowercase letters, digits, and single underscores: {source_name}"
      )
    )
  }

  if (nchar(source_name) > 30) {
    stop(glue::glue("source_name must be at most 30 characters: {source_name}"))
  }

  paste0("ms__", source_name)
}

db.get_market_source_data <- function(source_name, date_from = NULL, date_to = NULL) {
  table_name <- market_source_table_name(source_name)

  result <- db.pg_select(glue::glue("SELECT date, value, data_type FROM {table_name} ORDER BY date")) %>%
    mutate(
      date,
      value = as.numeric(value),
      data_type = as.character(data_type)
    )

  if (!is.null(date_from)) {
    result <- result %>% filter(date >= as.Date(date_from))
  }
  if (!is.null(date_to)) {
    result <- result %>% filter(date <= as.Date(date_to))
  }

  result
}

get_brent <- function() {
  log_info("Getting prices for Brent from database")
  brent <- db.get_market_source_data("eia_brent") %>%
    transmute(date, brent = value)

  if (nrow(brent) == 0) {
    stop("ms__eia_brent returned no rows")
  }

  brent %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    force_utc()
}


get_ttf <- function() {
  log_info("Getting prices for TTF from database")
  ttf <- db.get_market_source_data("oilprice_ttf") %>%
    transmute(
      date,
      ttf = as.numeric(value)
    )

  if (nrow(ttf) == 0) {
    stop("ms__oilprice_ttf returned no rows")
  }

  ttf %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    fill_past(date_from = "2020-01-01") %>%
    force_utc()
}

get_ara <- function() {
  log_info("Getting prices for ARA")
  ara <- db.get_market_source_data("bi_coal") %>%
    transmute(date, ara = value)

  if (nrow(ara) == 0) {
    stop("ms__bi_coal returned no rows")
  }

  ara %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    force_utc()
}


get_global_coal <- function() {
  log_info("Getting prices for global coal from database")
  result <- db.get_market_source_data("investing_com_coal") %>%
    transmute(
      date,
      global_coal = as.numeric(value)
    ) %>%
    select(date, global_coal)

  if (nrow(result) == 0) {
    stop("ms__raw__investing_com_coal returned no rows")
  }

  result %>%
    fill_gaps_and_future() %>%
    force_utc()
}

get_jkm <- function() {
  log_info("Getting prices for JKM from database")
  jkm <- db.get_market_source_data("oilprice_jkm") %>%
    transmute(date, jkm = value)

  if (nrow(jkm) == 0) {
    stop("ms__oilprice_jkm returned no rows")
  }

  jkm %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    fill_past(date_from = "2020-01-01") %>%
    force_utc()
}

get_prices_daily <- function(running_days = 0) {
  ttf_daily <- get_ttf()
  brent_daily <- get_brent()
  ara_daily <- get_ara()
  jkm <- get_jkm()
  global_coal_daily <- get_global_coal()
  eur_to_usd <- price.eur_per_usd() %>%
    select(date, eur_per_usd)

  ttf_daily %>%
    full_join(brent_daily, by = join_by(date)) %>%
    full_join(ara_daily, by = join_by(date)) %>%
    full_join(jkm, by = join_by(date)) %>%
    full_join(global_coal_daily, by = join_by(date)) %>%
    full_join(eur_to_usd, by = join_by(date)) %>%
    mutate(date = date %>% lubridate::force_tz("UTC")) %>%
    rcrea::utils.running_average(running_days, vars_to_avg = c("ttf", "brent", "ara", "jkm", "global_coal", "eur_per_usd"))
}

get_prices_monthly <- function() {
  ttf_monthly <- get_ttf() %>%
    group_by(date = date %>% "day<-"(1)) %>%
    summarise(ttf = mean(ttf, na.rm = TRUE))

  brent_monthly <- get_brent() %>%
    group_by(date = date %>% "day<-"(1)) %>%
    summarise(brent = mean(brent, na.rm = TRUE))

  ara_monthly <- get_ara() %>%
    group_by(date = date %>% "day<-"(1)) %>%
    summarise(ara = mean(ara, na.rm = TRUE))

  jkm_monthly <- get_jkm() %>%
    group_by(date = date %>% "day<-"(1)) %>%
    summarise(jkm = mean(jkm, na.rm = TRUE))

  global_coal_monthly <- get_global_coal() %>%
    group_by(date = date %>% "day<-"(1)) %>%
    summarise(global_coal = mean(global_coal, na.rm = TRUE))

  eur_to_usd <- price.eur_per_usd(monthly = T) %>%
    select(date, eur_per_usd)

  ttf_monthly %>%
    full_join(brent_monthly, by = join_by(date)) %>%
    full_join(ara_monthly, by = join_by(date)) %>%
    full_join(jkm_monthly, by = join_by(date)) %>%
    full_join(global_coal_monthly, by = join_by(date)) %>%
    full_join(eur_to_usd, by = join_by(date)) %>%
    force_utc()
}

price.eur_per_usd <- function(date_from = "2015-01-01", date_to = lubridate::today(), monthly = F) {
  log_info("Getting EUR per USD")
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)

  eur_per_usd <- db.get_market_source_data(
    source_name = "ecb_eur_usd",
    date_from = date_from,
    date_to = date_to
  ) %>%
    transmute(date, eur_per_usd = value)

  # Fill values - fail if no data
  if (nrow(eur_per_usd) == 0) {
    stop(glue::glue("No EUR per USD data returned from database for {date_from} to {date_to}"))
  }

  if (monthly) {
    eur_per_usd <- eur_per_usd %>%
      group_by(date = lubridate::floor_date(date, "month")) %>%
      summarise(eur_per_usd = mean(eur_per_usd, na.rm = T))
  }
  return(eur_per_usd)
}

price.cny_per_usd <- function(date_from = "2015-01-01", date_to = lubridate::today(), monthly = F) {
  log_info("Getting CNY per USD")
  date_from <- as.Date(date_from)
  date_to <- as.Date(date_to)

  eur_per_usd <- price.eur_per_usd(date_from = date_from, date_to = date_to, monthly = F)

  cny_per_eur <- db.get_market_source_data(
    source_name = "ecb_cny_eur",
    date_from = date_from,
    date_to = date_to
  ) %>%
    transmute(date, cny_per_eur = value)

  if (nrow(cny_per_eur) == 0) {
    stop(
      glue::glue(
        "No CNY per USD data returned for {date_from} to {date_to}"
      )
    )
  }

  cny_per_usd <- eur_per_usd %>%
    left_join(cny_per_eur, by = join_by(date)) %>%
    mutate(cny_per_usd = cny_per_eur * eur_per_usd) %>%
    select(date, cny_per_usd)

  # Fill values - fail if no data
  if (nrow(cny_per_usd) == 0) {
    stop(glue::glue("No CNY per USD data returned from database for {date_from} to {date_to}"))
  }

  if (any(is.na(cny_per_usd$cny_per_usd))) {
    stop(glue::glue("Missing CNY per USD values after joining {eur_usd_table_name} and {cny_eur_table_name}"))
  }

  if (monthly) {
    cny_per_usd <- cny_per_usd %>%
      group_by(date = lubridate::floor_date(date, "month")) %>%
      summarise(cny_per_usd = mean(cny_per_usd, na.rm = T))
  }
  return(cny_per_usd)
}

get_urals <- function() {
  log_info("Getting prices for Urals from database")
  # This should return a tibble of date, eur_per tonne

  prices <- db.get_market_source_data("oilprice_urals") %>%
    transmute(date, usd_per_bbl = value) %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    force_utc() %>%
    transmute(
      date,
      usd_per_bbl = usd_per_bbl
    )

  te_prices <- db.get_market_source_data("te_urals") %>%
    transmute(date, usd_per_bbl = value) %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    force_utc() %>%
    transmute(
      date,
      usd_per_bbl = usd_per_bbl
    )

  # Prefer oilprice.com data, but if it's missing for some dates, use TE data
  prices <- prices %>%
    full_join(te_prices, by = join_by(date), suffix = c("", "_te")) %>%
    mutate(usd_per_bbl = coalesce(usd_per_bbl, usd_per_bbl_te)) %>%
    select(date, usd_per_bbl)

  # If no prices available, fail
  if (nrow(prices) == 0) {
    stop("No Urals prices available from database (ms__oilprice_urals)")
  }

  eur_per_usd <- price.eur_per_usd(
    date_from = min(prices$date),
    date_to = max(prices$date)
  ) %>%
    fill_gaps_and_future()

  tonne_per_bbl <- 0.138

  return(
    prices %>%
      left_join(eur_per_usd, by = join_by(date)) %>%
      mutate(eur_per_tonne = usd_per_bbl * eur_per_usd / tonne_per_bbl) %>%
      select(date, eur_per_tonne) %>%
      mutate(commodity = "crude_oil_urals")
  )
}

get_espo <- function() {
  log_info("Getting prices for Espo from database")

  # The data for espo stops beyond mid-2024. We can estimate it based on the difference
  # between the most recently available espo and sokol prices.

  espo <- local({
    espo <- db.get_market_source_data("oilprice_espo") %>%
      dplyr::glimpse() %>%
      arrange(desc(date)) %>%
      force_utc() %>%
      transmute(
        date,
        usd_per_bbl = value,
        origin = data_type
      )

    sokol <- db.get_market_source_data("oilprice_sokol") %>%
      arrange(desc(date)) %>%
      force_utc() %>%
      transmute(
        date,
        usd_per_bbl = value,
        origin = data_type
      )

    urals <- db.get_market_source_data("oilprice_urals") %>%
      arrange(desc(date)) %>%
      force_utc() %>%
      transmute(
        date,
        usd_per_bbl = value,
        origin = data_type
      )

    urals_te <- db.get_market_source_data("te_urals") %>%
      arrange(desc(date)) %>%
      force_utc() %>%
      transmute(
        date,
        usd_per_bbl = value,
        origin = data_type
      )

    urals <- urals %>%
      full_join(urals_te, by = join_by(date), suffix = c("", "_te")) %>%
      mutate(usd_per_bbl = coalesce(usd_per_bbl, usd_per_bbl_te), origin = coalesce(origin, origin_te)) %>%
      select(date, usd_per_bbl, origin)

    # Get last date for original value espo
    last_espo <- espo %>%
      filter(origin == "original") %>%
      summarise(date = max(date)) %>%
      pull()

    # Get 30 days before that
    last_espo_minus_30 <- last_espo - lubridate::days(30)

    # Get the average difference between espo and sokol between last_espo_minus_30 and last_espo
    diff <- espo %>%
      filter(date >= last_espo_minus_30 & date <= last_espo) %>%
      left_join(
        sokol %>%
          filter(date >= last_espo_minus_30 & date <= last_espo),
        by = join_by(date),
        suffix = c(".espo", ".sokol")
      ) %>%
      mutate(diff = usd_per_bbl.espo - usd_per_bbl.sokol) %>%
      summarise(diff = mean(diff, na.rm = T)) %>%
      pull()

    # Estimate espo prices from sokol prices after last_espo
    estimated_espo_from_sokol <- sokol %>%
      filter(date > last_espo) %>%
      transmute(
        date = date,
        usd_per_bbl = usd_per_bbl + diff,
        origin = "estimated"
      )

    first_espo <- espo %>%
      filter(origin == "original") %>%
      summarise(date = min(date)) %>%
      pull()

    first_espo_plus_30 <- first_espo + lubridate::days(30)

    if (first_espo_plus_30 > as_date("2022-01-01")) {
      stop("First espo date must be before the start of 2022")
    }

    # Get the average difference between espo and urals between first_espo and first_espo_plus_30
    diff_from_urals <- espo %>%
      filter(date >= first_espo & date <= first_espo_plus_30) %>%
      left_join(
        urals %>%
          filter(date >= first_espo & date <= first_espo_plus_30),
        by = join_by(date),
        suffix = c(".espo", ".urals")
      ) %>%
      mutate(diff = usd_per_bbl.espo - usd_per_bbl.urals) %>%
      summarise(diff = mean(diff, na.rm = T)) %>%
      pull()

    # Estimate espo prices from urals prices before first_espo
    estimated_espo_from_urals <- urals %>%
      filter(date < first_espo) %>%
      transmute(
        date = date,
        usd_per_bbl = usd_per_bbl + diff_from_urals,
        origin = "estimated"
      )

    return(
      bind_rows(
        estimated_espo_from_urals,
        espo %>% filter(date <= last_espo & date >= first_espo),
        estimated_espo_from_sokol
      ) %>%
        select(date, usd_per_bbl) %>%
        arrange(desc(date)) %>%
        fill_gaps_and_future()
    )
  })

  # If no espo data available, fail
  if (nrow(espo) == 0) {
    stop("No Espo prices available from database (ms__oilprice_espo)")
  }

  eur_per_usd <- price.eur_per_usd(
    date_from = min(espo$date),
    date_to = max(espo$date)
  ) %>%
    fill_gaps_and_future()

  tonne_per_bbl <- 0.138

  # Combine original and estimated espo prices
  return(
    espo %>%
      left_join(eur_per_usd, by = join_by(date)) %>%
      mutate(eur_per_tonne = usd_per_bbl * eur_per_usd / tonne_per_bbl) %>%
      mutate(commodity = "crude_oil_espo") %>%
      select(commodity, date, eur_per_tonne) %>%
      arrange(desc(date))
  )
}
