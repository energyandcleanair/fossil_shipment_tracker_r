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

get_brent <- function() {
  log_info("Getting prices for Brent")
  brent_datahub1 <- tryCatch(
    {
      log_level(REQUEST, "Getting Brent prices from datahub")
      read_csv("https://datahub.io/core/oil-prices/_r/-/data/brent-daily.csv", show_col_type = F) %>%
        select(date = Date, brent = Price) %>%
        filter(date >= "2016-01-01") %>%
        arrange(desc(date))
    },
    error = function(e) {
      return(NULL)
    }
  )

  brent_datahub2 <- tryCatch(
    {
      log_level(REQUEST, "Getting Brent prices from datahub")
      read_csv("https://datahub.io/core/oil-prices/_r/-/data/brent-daily.csv", show_col_type = F) %>%
        select(date = Date, brent = Price) %>%
        filter(date >= "2016-01-01") %>%
        arrange(desc(date))
    },
    error = function(e) {
      return(NULL)
    }
  )

  brent_eia <- tryCatch(
    {
      log_level(REQUEST, "Getting Brent prices from EIA")
      eia::eia_series(
        id = "PET.RBRTE.D",
        start = lubridate::year(max(brent_datahub$date)),
        end = NULL,
        tidy = TRUE,
        cache = TRUE,
        key = Sys.getenv("EIA_KEY")
      ) %>%
        tidyr::unnest(data) %>%
        select(date, brent = value) %>%
        filter(date >= "2016-01-01")
    },
    error = function(e) {
      return(NULL)
    }
  )

  temp <- tempfile(fileext = ".xls")
  log_level(REQUEST, "Getting Brent prices from EIA")
  download.file("https://www.eia.gov/dnav/pet/hist_xls/RBRTEd.xls", temp, quiet = TRUE)
  brent_eia_xls <- readxl::read_xls(temp, sheet = "Data 1", skip = 3, col_names = c("date", "brent")) %>%
    mutate(date = as.Date(date)) %>%
    filter(date >= "2016-01-01") %>%
    arrange(desc(date))
  unlink(temp)

  bind_rows(
    brent_datahub1,
    brent_datahub2,
    brent_eia,
    brent_eia_xls
  ) %>%
    group_by(date) %>%
    summarise_at("brent", mean, na.rm = T) %>%
    filter(!is.na(brent)) %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    force_utc()
}


get_ttf <- function() {
  log_info("Getting prices for TTF from database")
  ttf <- db.pg_select("SELECT date, value AS ttf FROM ms__oilprice_ttf ORDER BY date")

  if (nrow(ttf) == 0) {
    stop("ms__oilprice_ttf returned no rows")
  }

  ttf %>%
    mutate(date = lubridate::as_datetime(date)) %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    fill_past(date_from = "2020-01-01") %>%
    force_utc()
}

get_ara <- function() {
  log_info("Getting prices for ARA")
  ara <- db.pg_select("SELECT date, value AS ara FROM ms__bi_coal ORDER BY date")

  if (nrow(ara) == 0) {
    stop("ms__bi_coal returned no rows")
  }

  ara %>%
    mutate(date = lubridate::as_datetime(date, tz = "UTC")) %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    force_utc()
}


get_global_coal <- function() {
  log_info("Getting prices for global coal")
  # tidyquant::tq_get("PCOALAUUSDM", get='economic.data', from='2015-01-01') %>%
  #   select(date, global_coal=price)
  url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTpPdsbpgUsmUU5MXDAH3Y0pg0HcR1_-fk-Flh_nPo0SRrUfOtno-l1627cgPIkvlMNlEjKTcF1dFF0/pub?gid=0&single=true&output=csv"
  log_level(REQUEST, "Getting global coal prices from Google Sheets")
  result <- read_csv(url) %>%
    mutate(
      date = strptime(Date, "%m/%d/%Y", tz = "UTC"),
      global_coal = as.numeric(Price)
    ) %>%
    select(date, global_coal)

  if (max(result$date) <= lubridate::today() - 7) {
    log_warn("Need to update global coal data here: https://docs.google.com/spreadsheets/d/1nQWZJuuUXyKn-hfd7besOXLlcKjqR7PqMLn4fvfJpzA/edit#gid=1784009253")
  }

  result %>%
    fill_gaps_and_future() %>%
    force_utc()
}

get_jkm <- function() {
  log_info("Getting prices for JKM from database")
  jkm <- db.pg_select("SELECT date, value AS jkm FROM ms__oilprice_jkm ORDER BY date")

  if (nrow(jkm) == 0) {
    stop("ms__oilprice_jkm returned no rows")
  }

  jkm %>%
    mutate(date = lubridate::as_datetime(date)) %>%
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
    mutate(date = lubridate::date(date) %>% lubridate::force_tz("UTC")) %>%
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
  get_from_european_central_bank <- function(date_from, date_to) {
    log_level(REQUEST, glue::glue("Getting EUR per USD from European Central Bank for {date_from} to {date_to}"))
    read_csv("https://data-api.ecb.europa.eu/service/data/EXR/D.USD.EUR.SP00.A?format=csvdata", show_col_types = FALSE) %>%
      mutate(
        date = lubridate::ymd(TIME_PERIOD),
        eur_per_usd = 1 / OBS_VALUE
      ) %>%
      filter(date_from <= date & date <= date_to) %>%
      select(date, eur_per_usd)
  }

  eur_per_usd <- get_from_european_central_bank(date_from, date_to)

  # Fill values - fail if no data
  if (nrow(eur_per_usd) == 0) {
    stop(glue::glue("No EUR per USD data returned from European Central Bank for {date_from} to {date_to}"))
  }

  eur_per_usd <- eur_per_usd %>%
    tidyr::complete(date = seq.Date(min(.$date), max(.$date, date(date_to)), by = "day")) %>%
    tidyr::fill(eur_per_usd)

  if (monthly) {
    eur_per_usd <- eur_per_usd %>%
      group_by(date = lubridate::floor_date(date, "month")) %>%
      summarise(eur_per_usd = mean(eur_per_usd, na.rm = T))
  }
  return(eur_per_usd)
}

price.cny_per_usd <- function(date_from = "2015-01-01", date_to = lubridate::today(), monthly = F) {
  log_info("Getting CNY per USD")
  get_from_european_central_bank <- function(date_from, date_to) {
    log_level(REQUEST, glue::glue("Getting EUR per USD from European Central Bank for {date_from} to {date_to}"))
    eur_per_usd <- read_csv("https://data-api.ecb.europa.eu/service/data/EXR/D.USD.EUR.SP00.A?format=csvdata", show_col_types = FALSE) %>%
      mutate(
        date = lubridate::ymd(TIME_PERIOD),
        eur_per_usd = 1 / OBS_VALUE # API returns USD per EUR
      ) %>%
      filter(date_from <= date & date <= date_to) %>%
      select(date, eur_per_usd)

    log_level(REQUEST, glue::glue("Getting CNY per EUR from European Central Bank for {date_from} to {date_to}"))
    cyn_per_eur <- read_csv("https://data-api.ecb.europa.eu/service/data/EXR/D.CNY.EUR.SP00.A?format=csvdata", show_col_types = FALSE) %>%
      mutate(
        date = lubridate::ymd(TIME_PERIOD),
        cyn_per_eur = OBS_VALUE # API returns CNY per EUR
      ) %>%
      filter(date_from <= date & date <= date_to) %>%
      select(date, cyn_per_eur)

    return(
      eur_per_usd %>%
        left_join(cyn_per_eur, by = join_by(date)) %>%
        mutate(cny_per_usd = cyn_per_eur * eur_per_usd) %>% # Cancels out the EUR
        select(date, cny_per_usd)
    )
  }
  cny_per_usd <- get_from_european_central_bank(date_from, date_to)

  # Fill values - fail if no data
  if (nrow(cny_per_usd) == 0) {
    stop(glue::glue("No CNY per USD data returned from European Central Bank for {date_from} to {date_to}"))
  }

  cny_per_usd <- cny_per_usd %>%
    tidyr::complete(date = seq.Date(min(.$date), max(.$date, as.Date(date_to)), by = "day")) %>%
    tidyr::fill(cny_per_usd)

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

  prices <- db.pg_select("SELECT date, value AS usd_per_bbl FROM ms__oilprice_urals ORDER BY date") %>%
    mutate(date = lubridate::as_datetime(date)) %>%
    arrange(desc(date)) %>%
    fill_gaps_and_future() %>%
    force_utc() %>%
    transmute(
      date = lubridate::as_date(date),
      usd_per_bbl = usd_per_bbl
    )

  # If no prices available, fail
  if (nrow(prices) == 0) {
    stop("No Urals prices available from database (ms__oilprice_urals)")
  }

  eur_per_usd <- price.eur_per_usd(
    date_from = min(prices$date),
    date_to = max(prices$date)
  ) %>%
    mutate(
      # to dttm
      date = lubridate::as_date(date)
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
    espo <- db.pg_select("SELECT date, value AS usd_per_bbl FROM ms__oilprice_espo ORDER BY date") %>%
      mutate(date = lubridate::as_datetime(date)) %>%
      arrange(desc(date)) %>%
      fill_gaps_and_future() %>%
      force_utc() %>%
      transmute(
        date = lubridate::as_date(date),
        usd_per_bbl = usd_per_bbl,
        origin = "original"
      )

    sokol <- db.pg_select("SELECT date, value AS usd_per_bbl FROM ms__oilprice_sokol ORDER BY date") %>%
      mutate(date = lubridate::as_datetime(date)) %>%
      arrange(desc(date)) %>%
      fill_gaps_and_future() %>%
      force_utc() %>%
      transmute(
        date = lubridate::as_date(date),
        usd_per_bbl = usd_per_bbl,
        origin = "original"
      )

    urals <- db.pg_select("SELECT date, value AS usd_per_bbl FROM ms__oilprice_urals ORDER BY date") %>%
      mutate(date = lubridate::as_datetime(date)) %>%
      arrange(desc(date)) %>%
      fill_gaps_and_future() %>%
      force_utc() %>%
      transmute(
        date = lubridate::as_date(date),
        usd_per_bbl = usd_per_bbl,
        origin = "original"
      )

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
    mutate(
      # to dttm
      date = lubridate::as_date(date)
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
