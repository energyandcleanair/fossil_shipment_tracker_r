fill_past <- function(result, date_from) {
  result %>%
    ungroup() %>%
    tidyr::complete(date = seq(as.Date(date_from), lubridate::date(min(date)), by = "day")) %>%
    arrange(desc(date)) %>%
    tidyr::fill(setdiff(names(.), "date"), .direction = "down")
}

fill_gaps_and_future <- function(result) {
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


  #   brent_yahoo <- quantmod::getSymbols("BZ=F", from = '2022-12-01', warnings = FALSE, auto.assign = F) %>%
  #     as.data.frame() %>%
  #     mutate(date = gsub("X","",gsub("\\.","-",rownames(.))) %>% ymd) %>%
  #     tibble() %>%
  #     rename(brent = contains('Close')) %>%
  #     filter(!is.na(date)) %>%
  #     select(date, brent)

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
  log_info("Getting prices for TTF")
  oilprice.get_price_for_oil("ttf", end_date = today()) %>%
    select(
      date,
      ttf = value_dpb,
    ) %>%
    fill_gaps_and_future() %>%
    fill_past(date_from = "2020-01-01") %>%
    force_utc() # missing 14 days
}

get_ara <- function() {
  log_info("Getting prices for ARA")
  ara_historical <- readr::read_csv(system.file("extdata", "Rotterdam_Coal_Futures_Historical_Data.csv", package = "russiacounter")) %>%
    mutate(
      date = strptime(Date, "%b %d, %Y", tz = "UTC"),
      ara = as.numeric(Price)
    ) %>%
    select(date, ara)

  url <- "https://www.theice.com/marketdata/DelayedMarkets.shtml?getHistoricalChartDataAsJson=&marketId=5310593&historicalSpan=3"

  log_level(REQUEST, "Getting ARA prices from ICE")
  ara_new <- jsonlite::fromJSON(url)$bars %>%
    as.data.frame() %>%
    `names<-`(c("date", "ara")) %>%
    tibble() %>%
    mutate(
      date = strptime(date, "%a %b %d %H:%M:%S %Y", tz = "UTC"),
      ara = as.numeric(ara)
    ) %>%
    arrange(desc(date))

  ara <- bind_rows(
    ara_historical,
    ara_new %>% filter(date >= max(ara_historical$date))
  )

  # Fill gaps and next 7 days
  ara %>%
    fill_gaps_and_future() %>%
    force_utc()
}

get_newcastle <- function() {
  log_info("Getting prices for Newcastle")
  url <- "https://www.theice.com/marketdata/DelayedMarkets.shtml?getHistoricalChartDataAsJson=&marketId=5310550&historicalSpan=3"
  log_level(REQUEST, "Getting Newcastle prices from ICE")
  newcastle <- jsonlite::fromJSON(url)$bars

  as.data.frame(newcastle) %>%
    `names<-`(c("date", "newcastle")) %>%
    tibble() %>%
    mutate(
      date = strptime(date, "%a %b %d %H:%M:%S %Y", tz = "UTC"),
      newcastle = as.numeric(newcastle)
    ) %>%
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
  log_info("Getting prices for JKM")
  # url <- "https://assets.ino.com/data/history/?s=NYMEX_QJKM.K22&b=&f=json"
  # jkm <- jsonlite::fromJSON(url)
  # as.data.frame(jkm) %>%
  #   `names<-`(c("date", "open", "high", "low", "close", "volume")) %>%
  #   tibble() %>%
  #   mutate(date=as.POSIXct(date/1000, origin="1970-01-01")) %>%
  #   select(date, jkm=close) %>%
  oilprice.get_price_for_oil("jkm", end_date = today()) %>%
    select(
      date,
      jkm = value_dpb
    ) %>%
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
    summarise(across(ttf, mean, na.rm = T))

  brent_monthly <- get_brent() %>%
    group_by(date = date %>% "day<-"(1)) %>%
    summarise(across(brent, mean, na.rm = T))

  ara_monthly <- get_ara() %>%
    group_by(date = date %>% "day<-"(1)) %>%
    summarise(across(ara, mean, na.rm = T))

  jkm_monthly <- get_jkm() %>%
    group_by(date = date %>% "day<-"(1)) %>%
    summarise(across(jkm, mean, na.rm = T))

  global_coal_monthly <- get_global_coal() %>%
    group_by(date = date %>% "day<-"(1)) %>%
    summarise(across(global_coal, mean, na.rm = T))

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
    log_level(REQUEST, "Getting EUR per USD from European Central Bank")
    read_csv("https://data-api.ecb.europa.eu/service/data/EXR/D.USD.EUR.SP00.A?format=csvdata", show_col_types = FALSE) %>%
      mutate(
        date = lubridate::ymd(TIME_PERIOD),
        eur_per_usd = 1 / OBS_VALUE
      ) %>%
      filter(date_from <= date & date <= date_to) %>%
      select(date, eur_per_usd)
  }

  eur_per_usd <- get_from_european_central_bank(date_from, date_to)

  # Fill values
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
    log_level(REQUEST, "Getting EUR per USD from European Central Bank")
    eur_per_usd <- read_csv("https://data-api.ecb.europa.eu/service/data/EXR/D.USD.EUR.SP00.A?format=csvdata", show_col_types = FALSE) %>%
      mutate(
        date = lubridate::ymd(TIME_PERIOD),
        eur_per_usd = 1 / OBS_VALUE # API returns USD per EUR
      ) %>%
      filter(date_from <= date & date <= date_to) %>%
      select(date, eur_per_usd)

    log_level(REQUEST, "Getting CYN per EUR from European Central Bank")
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

  # Fill values
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
  log_info("Getting prices for Urals")
  # This should return a tibble of date, eur_per tonne

  prices <- oilprice.get_price_for_oil("urals", end_date = lubridate::today() + lubridate::days(7)) %>%
    fill_gaps_and_future() %>%
    force_utc() %>%
    transmute(
      date = lubridate::as_date(date),
      usd_per_bbl = value_dpb
    )

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
  log_info("Getting prices for Espo")

  # The data for espo stops beyond mid-2024. We can estimate it based on the difference
  # between the most recently available espo and sokol prices.

  espo <- local({
    espo <- oilprice.get_price_for_oil("espo", end_date = lubridate::today() + lubridate::days(7)) %>%
      fill_gaps_and_future() %>%
      force_utc() %>%
      transmute(
        date = lubridate::as_date(date),
        usd_per_bbl = value_dpb,
        origin = origin
      )
    sokol <- oilprice.get_price_for_oil("sokol", end_date = lubridate::today() + lubridate::days(7)) %>%
      fill_gaps_and_future() %>%
      force_utc() %>%
      transmute(
        date = lubridate::as_date(date),
        usd_per_bbl = value_dpb,
        origin = origin
      )
    urals <- oilprice.get_price_for_oil("urals", end_date = lubridate::today() + lubridate::days(7)) %>%
      fill_gaps_and_future() %>%
      force_utc() %>%
      transmute(
        date = lubridate::as_date(date),
        usd_per_bbl = value_dpb,
        origin = origin
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
      mutate(
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
      mutate(
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
