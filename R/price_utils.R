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
  brent_datahub1 <- tryCatch(
    {
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
  # quantmod::getSymbols("TTF=F", from = '2016-01-01', warnings = FALSE, auto.assign = F) %>%
  #   as.data.frame() %>%
  #   mutate(date = gsub("X","",gsub("\\.","-",rownames(.))) %>% ymd) %>%
  #   tibble() %>%
  #   rename(ttf = contains('Close')) %>%
  #   select(date, ttf) %>%
  #   fill_gaps_and_future()


  # url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTpPdsbpgUsmUU5MXDAH3Y0pg0HcR1_-fk-Flh_nPo0SRrUfOtno-l1627cgPIkvlMNlEjKTcF1dFF0/pub?gid=1776269924&single=true&output=csv"
  # historical <- read_csv(url, show_col_type=F) %>%
  #   mutate(date = strptime(Date, "%m/%d/%Y", tz="UTC"),
  #          ttf = as.numeric(Price)) %>%
  #   select(date, ttf)

  oilprice.get_prices("ttf") %>%
    fill_gaps_and_future() %>%
    fill_past(date_from = "2020-01-01") %>%
    force_utc() # missing 14 days
}

get_ara <- function() {
  ara_historical <- readr::read_csv(system.file("extdata", "Rotterdam_Coal_Futures_Historical_Data.csv", package = "russiacounter")) %>%
    mutate(
      date = strptime(Date, "%b %d, %Y", tz = "UTC"),
      ara = as.numeric(Price)
    ) %>%
    select(date, ara)

  url <- "https://www.theice.com/marketdata/DelayedMarkets.shtml?getHistoricalChartDataAsJson=&marketId=5310593&historicalSpan=3"

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
  url <- "https://www.theice.com/marketdata/DelayedMarkets.shtml?getHistoricalChartDataAsJson=&marketId=5310550&historicalSpan=3"
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
  # tidyquant::tq_get("PCOALAUUSDM", get='economic.data', from='2015-01-01') %>%
  #   select(date, global_coal=price)
  url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTpPdsbpgUsmUU5MXDAH3Y0pg0HcR1_-fk-Flh_nPo0SRrUfOtno-l1627cgPIkvlMNlEjKTcF1dFF0/pub?gid=0&single=true&output=csv"
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


# get_asia_lng <- function(){
#   # tidyquant::tq_get("PNGASJPUSDM", get='economic.data', from='2015-01-01') %>%
#   #   select(date, asia_lng=price)
#   # url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTpPdsbpgUsmUU5MXDAH3Y0pg0HcR1_-fk-Flh_nPo0SRrUfOtno-l1627cgPIkvlMNlEjKTcF1dFF0/pub?gid=1027875093&single=true&output=csv"
#   # result <- read_csv(url) %>%
#   #   mutate(date = strptime(Date, "%m/%d/%Y", tz="UTC"),
#   #          asia_lng = as.numeric(Price)) %>%
#   #   select(date, asia_lng)
#   #
#   # if(max(result$date) <= lubridate::today() - 7){
#   #   warning("Need to update global coal data here: https://docs.google.com/spreadsheets/d/1nQWZJuuUXyKn-hfd7besOXLlcKjqR7PqMLn4fvfJpzA/edit#gid=1784009253")
#   # }
#   oilprice.get_prices("jkm") %>%
#     fill_gaps_and_future()
# }


get_jkm <- function() {
  # url <- "https://assets.ino.com/data/history/?s=NYMEX_QJKM.K22&b=&f=json"
  # jkm <- jsonlite::fromJSON(url)
  # as.data.frame(jkm) %>%
  #   `names<-`(c("date", "open", "high", "low", "close", "volume")) %>%
  #   tibble() %>%
  #   mutate(date=as.POSIXct(date/1000, origin="1970-01-01")) %>%
  #   select(date, jkm=close) %>%
  oilprice.get_prices("jkm") %>%
    fill_gaps_and_future() %>%
    fill_past(date_from = "2020-01-01") %>%
    force_utc()
}

get_refinery_margin <- function() {
  url <- "https://data.nasdaq.com/api/v3/datasets/BP/OIL_REF_MARG.csv"

  cache_path <- "cache/BP-OIL_REF_MARG.csv"
  inst_path <- system.file("extdata", "BP-OIL_REF_MARG.csv", package = "russiacounter")

  if (!file.exists(cache_path)) {
    dir.create("cache", F)
    file.copy(inst_path, cache_path)
  }

  refinery_raw <- tryCatch(
    {
      d <- read_csv(url)
      write_csv(d, cache_path)
      # Just for regular updates to be embedded in the package
      try(write_csv(d, file.path("inst/extdata/BP-OIL_REF_MARG.csv")))
      d
    },
    error = function(error) {
      return(read_csv(cache_path))
    }
  )

  refinery <- refinery_raw %>%
    rename(
      date = Date,
      refining_heavy = `USGC Heavy Sour Coking`,
      refining_medium = `Singapore Medium Sour Hydrocracking`,
      refining_light = `NWE Light Sweet Cracking`
    )

  refinery %>%
    mutate(date = lubridate::floor_date(date, "month")) %>%
    group_by(date) %>%
    summarise(
      refining_heavy = mean(refining_heavy, na.rm = TRUE),
      refining_medium = mean(refining_medium, na.rm = TRUE),
      refining_light = mean(refining_light, na.rm = TRUE)
    ) %>%
    fill_gaps_and_future() %>%
    filter(date >= "2015-01-01") %>%
    force_utc()
}

get_prices_daily <- function(running_days = 0) {
  ttf_daily <- get_ttf()
  brent_daily <- get_brent()
  ara_daily <- get_ara()
  jkm <- get_jkm()
  global_coal_daily <- get_global_coal()
  refinery <- get_refinery_margin()

  ttf_daily %>%
    full_join(brent_daily, by = join_by(date)) %>%
    full_join(ara_daily, by = join_by(date)) %>%
    full_join(jkm, by = join_by(date)) %>%
    full_join(global_coal_daily, by = join_by(date)) %>%
    full_join(refinery, by = join_by(date)) %>%
    mutate(date = lubridate::date(date) %>% lubridate::force_tz("UTC")) %>%
    rcrea::utils.running_average(running_days, vars_to_avg = c("ttf", "brent", "ara", "jkm", "global_coal"))
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

  refining_monthly <- get_refinery_margin() %>%
    group_by(date = as.Date(date) %>% "day<-"(1)) %>%
    summarise(
      across(refining_heavy, mean, na.rm = T),
      across(refining_medium, mean, na.rm = T),
      across(refining_light, mean, na.rm = T)
    )

  ttf_monthly %>%
    full_join(brent_monthly, by = join_by(date)) %>%
    full_join(ara_monthly, by = join_by(date)) %>%
    full_join(jkm_monthly, by = join_by(date)) %>%
    full_join(global_coal_monthly, by = join_by(date)) %>%
    full_join(refining_monthly, by = join_by(date)) %>%
    force_utc()
}

price.eur_per_usd <- function(date_from = "2015-01-01", date_to = lubridate::today(), monthly = F) {
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
  oilprice.get_prices("urals") %>%
    fill_gaps_and_future()
}

get_ural_brent_spread <- function() {
  url <- "https://ir-service.appspot.com/share/nesteoil/English/price_monitor3_dg.html?name=Urals-Brent"

  webpage <- read_html(url)
  results <- webpage %>%
    html_nodes("script") %>%
    lapply(function(x) html_text(x, trim = TRUE))
  result <- results[[which(stringr::str_detect(results, "google.visualization.DataTable"))]]

  a <- gsub(".*rows: \\[ ", "", result)
  b <- gsub(";.*", "", a)
  c <- gsub("\\}, \\{c: ", "", b)
  d <- gsub("\\{c: ", "", c)
  e <- gsub("} ] }.*", "", d)
  rows <- stringr::str_split(e, "\\]\\[")[[1]]
  rows_numbers <- str_extract_all(rows, "[-]?[\\d|\\.]+", simplify = T) %>%
    apply(c(1, 2), as.numeric)
  dates <- lubridate::make_date(rows_numbers[, 1], rows_numbers[, 2] + 1, rows_numbers[, 3])
  values_usd_per_bbl <- rows_numbers[, 4]

  # eur_per_usd <- price.eur_per_usd(date_from=min(dates), date_to=max(dates))
  # tonne_per_bbl <- 0.138
  result <- tibble(date = dates, usd_per_bbl = values_usd_per_bbl) %>%
    select(date, usd_per_bbl)

  # Fill with last 7 days average
  completion <- tidyr::crossing(
    date = seq.Date(max(result$date) + lubridate::days(1), max(result$date, lubridate::today(tz = "UTC")) + lubridate::days(7), by = "day"),
    usd_per_bbl = result %>% arrange(date) %>% tail(7) %>% pull(usd_per_bbl) %>% mean()
  )

  result <- bind_rows(
    result %>% arrange(date),
    completion
  )

  return(result)

  # left_join(eur_per_usd) %>%
  # mutate(reduction_eur_per_tonne=-values_usd_per_bbl * eur_per_usd / tonne_per_bbl,
  #        commodity="crude_oil") %>%
  # select(commodity, date, reduction_eur_per_tonne)
}


get_espo_brent_spread <- function() {
  date_to <- lubridate::today() + lubridate::days(7)
  get_at_date <- function(target_date) {
    espo_discount <- tibble(
      date = c(
        "1990-01-01", "2022-02-24", "2022-03-11", "2022-03-17",
        "2022-04-11", "2022-05-04", "2022-05-29", "2022-06-24", "2022-07-08", "2022-08-08", as.character(date_to)
      ),
      discount = c(0, 0, 20, 26, 30, 30, 39, 37, 25, 25, 20)
    )
    approx(as_date(espo_discount$date), espo_discount$discount, as_date(target_date))$y
  }
  dates <- seq.Date(as.Date("2020-01-01"), date_to, by = "day")
  # eur_per_usd <- price.eur_per_usd(date_from=min(dates), date_to=max(dates))
  # tonne_per_bbl <- 0.138
  tibble(date = dates, usd_per_bbl = -get_at_date(date)) %>%
    select(date, usd_per_bbl) %>%
    arrange(desc(date))


  # left_join(eur_per_usd) %>%
  # mutate(reduction_eur_per_tonne=-values_usd_per_bbl * eur_per_usd / tonne_per_bbl,
  #        commodity="crude_oil") %>%
  # select(commodity, date, reduction_eur_per_tonne)
}

get_urals <- function(brent = NULL, eur_per_usd = NULL) {
  if (is.null(brent)) {
    brent <- get_brent()
  }

  if (is.null(eur_per_usd)) {
    eur_per_usd <- price.eur_per_usd(
      date_from = min(lubridate::date(brent$date)),
      date_to = min(lubridate::date(max(brent$date)), lubridate::today())
    )
  }

  spread_ural <- russiacounter::get_ural_brent_spread() %>%
    select(date, add_brent = usd_per_bbl) %>%
    tidyr::complete(date = seq.Date(min(date), max(date), by = "day")) %>%
    fill(add_brent)

  tonne_per_bbl <- 0.138

  brent %>%
    left_join(spread_ural, by = join_by(date)) %>%
    arrange(desc(date)) %>%
    mutate(usd_per_bbl = brent + add_brent) %>%
    left_join(eur_per_usd, by = join_by(date)) %>%
    mutate(eur_per_tonne = usd_per_bbl * eur_per_usd / tonne_per_bbl) %>%
    select(date, eur_per_tonne) %>%
    mutate(commodity = "crude_oil")
}

get_espo <- function(brent = NULL, eur_per_usd = NULL) {
  if (is.null(brent)) {
    brent <- get_brent()
  }

  if (is.null(eur_per_usd)) {
    eur_per_usd <- price.eur_per_usd(
      date_from = min(lubridate::date(brent$date)),
      date_to = min(lubridate::date(max(brent$date)), lubridate::today())
    )
  }

  spread_espo <- get_espo_brent_spread() %>%
    select(date, add_brent = usd_per_bbl) %>%
    arrange(date) %>%
    tidyr::complete(date = seq.Date(min(date), max(date), by = "day")) %>%
    fill(add_brent)

  tonne_per_bbl <- 0.138

  brent %>%
    left_join(spread_espo, by = join_by(date)) %>%
    arrange(desc(date)) %>%
    mutate(usd_per_bbl = brent + add_brent) %>%
    left_join(eur_per_usd, by = join_by(date)) %>%
    mutate(eur_per_tonne = usd_per_bbl * eur_per_usd / tonne_per_bbl) %>%
    select(date, eur_per_tonne) %>%
    mutate(commodity = "crude_oil")
}
#'
#' price.ttf <- function(date_from="2016-01-01"){
#'   p <- quantmod::getSymbols("TTF=F",
#'                             from = date_from,
#'                             warnings = FALSE,
#'                             auto.assign = F)
#'   tibble(date=index(p),
#'          price=as.numeric(coredata(p$`TTF=F.Adjusted`)[]),
#'          unit="EUR/MWh")
#' }
#'
#'
#' price.brent<- function(date_from="2020-01-01"){
#'   p <- tidyquant::tq_get("BZ=F", from=date_from)
#'   p %>%
#'     select(date, value=adjusted) %>%
#'     mutate(unit="USD/bbl") %>%
#'     drop_na()
#' }
#'
#'
#'
#'
#' price.natural_gas_based_on_brent<- function(date_from="2018-01-01"){
#'   # oil price of $69/barrel = $280 / 1000 m3 for gas; contracts are tied to oil
#'   # https://www.reuters.com/markets/europe/russias-oil-gas-revenue-windfall-2022-01-21/
#'   eur_per_usd <- price.eur_per_usd(date_from=date_from)
#'
#'   tidyquant::tq_get("BZ=F", from=date_from) %>%
#'     select(date, value=adjusted) %>% #USD/bbl
#'     left_join(eur_per_usd) %>%
#'     mutate(value=value* 280 / 69 /1000 / gcv_MWh_per_m3 * eur_per_usd) %>%
#'     mutate(unit="EUR/MWh") %>%
#'     select(-c(eur_per_usd)) %>%
#'     rcrea::utils.running_average(30) %>%
#'     mutate(commodity="natural_gas")
#'
#' }
#'
#'
#' #' Price based on Comtrade data, which is given both in kg and USD
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' price.comtrade <- function(){
#'   flows <- comtrade.get_flows(use_cache=F)
#'
#'   flows %>%
#'     filter(unit=="MWh/month") %>%
#'     mutate(value_usd_per_mwh=value_usd/value) %>%
#'     ggplot() +
#'     geom_line(aes(date, value_usd_per_mwh, col=commodity)) +
#'     facet_wrap(~country, scales="free")
#' }
