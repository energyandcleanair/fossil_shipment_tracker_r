fill_gaps_and_future <- function(result, n_days=7){
  result %>%
    ungroup() %>%
    tidyr::complete(date = seq(min(date), max(date) + lubridate::days(n_days), by = "day")) %>%
    arrange(desc(date)) %>%
    tidyr::fill(setdiff(names(.),"date"), .direction="up")
}

get_brent <- function(){
  quantmod::getSymbols("BZ=F", from = '2016-01-01', warnings = FALSE, auto.assign = F) %>%
    as.data.frame() %>%
    mutate(date = gsub("X","",gsub("\\.","-",rownames(.))) %>% ymd) %>%
    tibble() %>%
    rename(brent = contains('Close')) %>%
    filter(!is.na(date)) %>%
    select(date, brent) %>%
    fill_gaps_and_future()
}


get_ttf <- function(){
  quantmod::getSymbols("TTF=F", from = '2016-01-01', warnings = FALSE, auto.assign = F) %>%
    as.data.frame() %>%
    mutate(date = gsub("X","",gsub("\\.","-",rownames(.))) %>% ymd) %>%
    tibble() %>%
    rename(ttf = contains('Close')) %>%
    select(date, ttf) %>%
    fill_gaps_and_future()
}

get_ara <- function(){
  ara_historical <- readr::read_csv(system.file("extdata", "Rotterdam_Coal_Futures_Historical_Data.csv", package="russiacounter")) %>%
    mutate(date=strptime(Date, "%b %d, %Y", tz="UTC"),
           ara = as.numeric(Price)) %>%
    select(date, ara)


  url <- "https://www.theice.com/marketdata/DelayedMarkets.shtml?getHistoricalChartDataAsJson=&marketId=5310587&historicalSpan=2"
  ara_new <- jsonlite::fromJSON(url)$bars %>%
    as.data.frame() %>%
    `names<-`(c("date", "ara")) %>%
    tibble() %>%
    mutate(date = strptime(date, "%a %b %d %H:%M:%S %Y", tz="UTC"),
           ara = as.numeric(ara))

  ara <- bind_rows(ara_historical,
                      ara_new %>% filter(date >= max(ara_historical$date)))

  # Fill gaps and next 7 days
  ara %>%
    fill_gaps_and_future()
}

get_newcastle <- function(){
  url <- "https://www.theice.com/marketdata/DelayedMarkets.shtml?getHistoricalChartDataAsJson=&marketId=5310550&historicalSpan=3"
  newcastle <- jsonlite::fromJSON(url)$bars

 as.data.frame(newcastle) %>%
    `names<-`(c("date", "newcastle")) %>%
    tibble() %>%
    mutate(date = strptime(date, "%a %b %d %H:%M:%S %Y", tz="UTC"),
           newcastle = as.numeric(newcastle))  %>%
    fill_gaps_and_future()
}


get_global_coal <- function(){
  # tidyquant::tq_get("PCOALAUUSDM", get='economic.data', from='2015-01-01') %>%
  #   select(date, global_coal=price)
  url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTpPdsbpgUsmUU5MXDAH3Y0pg0HcR1_-fk-Flh_nPo0SRrUfOtno-l1627cgPIkvlMNlEjKTcF1dFF0/pub?gid=0&single=true&output=csv"
  result <- read_csv(url) %>%
    mutate(date = strptime(Date, "%b %d, %Y", tz="UTC"),
           global_coal = as.numeric(Price)) %>%
    select(date, global_coal)

  if(max(result$date) <= lubridate::today() - 7){
    warning("Need to update global coal data here: https://docs.google.com/spreadsheets/d/1nQWZJuuUXyKn-hfd7besOXLlcKjqR7PqMLn4fvfJpzA/edit#gid=1784009253")
  }

  result  %>%
    fill_gaps_and_future()
}


get_asia_lng <- function(){
  # tidyquant::tq_get("PNGASJPUSDM", get='economic.data', from='2015-01-01') %>%
  #   select(date, asia_lng=price)
  url <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vTpPdsbpgUsmUU5MXDAH3Y0pg0HcR1_-fk-Flh_nPo0SRrUfOtno-l1627cgPIkvlMNlEjKTcF1dFF0/pub?gid=1027875093&single=true&output=csv"
  result <- read_csv(url) %>%
    mutate(date = strptime(Date, "%b %d, %Y", tz="UTC"),
           asia_lng = as.numeric(Price)) %>%
    select(date, asia_lng)

  if(max(result$date) <= lubridate::today() - 7){
    warning("Need to update global coal data here: https://docs.google.com/spreadsheets/d/1nQWZJuuUXyKn-hfd7besOXLlcKjqR7PqMLn4fvfJpzA/edit#gid=1784009253")
  }

  result  %>%
    fill_gaps_and_future()
}


get_jkm <- function(){
  url <- "https://assets.ino.com/data/history/?s=NYMEX_QJKM.K22&b=&f=json"
  jkm <- jsonlite::fromJSON(url)
  as.data.frame(jkm) %>%
    `names<-`(c("date", "open", "high", "low", "close", "volume")) %>%
    tibble() %>%
    mutate(date=as.POSIXct(date/1000, origin="1970-01-01")) %>%
    select(date, jkm=close) %>%
    fill_gaps_and_future()
}


get_prices_monthly <- function(){

  ttf_monthly <- get_ttf() %>%
    group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(ttf, mean, na.rm=T))

  brent_monthly <- get_brent() %>%
    group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(brent, mean, na.rm=T))

  ara_monthly <- get_ara() %>%
    group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(ara, mean, na.rm=T))

  asia_lng_monthly <- get_asia_lng() %>%
    group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(asia_lng, mean, na.rm=T))

  global_coal_monthly <- get_global_coal() %>%
    group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(global_coal, mean, na.rm=T))

  ttf_monthly %>%
    full_join(brent_monthly) %>%
    full_join(ara_monthly) %>%
    full_join(asia_lng_monthly) %>%
    full_join(global_coal_monthly) %>%
    mutate(date=lubridate::date(date) %>% lubridate::force_tz("UTC"))
}

price.eur_per_usd <- function(date_from="2015-01-01", date_to=lubridate::today(), monthly=F){
  eur_per_usd <- priceR::historical_exchange_rates("USD", to = "EUR",
                                                   start_date = date_from,
                                                   end_date = date_to)
  eur_per_usd <- tibble(eur_per_usd) %>% `names<-`(c("date","eur_per_usd"))

  if(monthly){
    eur_per_usd %>%
      group_by(lubridate::floor_date(date, "month")) %>%
      summarise(eur_per_usd=mean(eur_per_usd, na.rm=T))
  }
  return(eur_per_usd)
}

get_ural_brent_spread <- function(){
  url <- "https://ir-service.appspot.com/share/nesteoil/English/price_monitor3_dg.html?name=Urals-Brent"
  library(rvest)
  library(tidyverse)

  webpage <- read_html(url)
  results <- webpage %>% html_nodes("script") %>% lapply(function(x) html_text(x, trim = TRUE))
  result <- results[[which(stringr::str_detect(results, "google.visualization.DataTable"))]]

  a <- gsub(".*rows: \\[ ","",result)
  b <- gsub(";.*","",a)
  c <- gsub("\\}, \\{c: ","" ,b)
  d <- gsub("\\{c: ","" ,c)
  e <- gsub("} ] }.*","" ,d)
  rows <- stringr::str_split(e, "\\]\\[")[[1]]
  rows_numbers <- str_extract_all(rows, "[-]?[\\d|\\.]+", simplify = T) %>%
    apply(c(1,2), as.numeric)
  dates <- lubridate::make_date(rows_numbers[,1], rows_numbers[,2]+1, rows_numbers[,3])
  values_usd_per_bbl <- rows_numbers[,4]

  # eur_per_usd <- price.eur_per_usd(date_from=min(dates), date_to=max(dates))
  # tonne_per_bbl <- 0.138
  result <- tibble(date=dates, usd_per_bbl=values_usd_per_bbl) %>%
    select(date, usd_per_bbl)

  # Fill with last 7 days average
  completion <- tidyr::crossing(date = seq.Date(max(result$date) + lubridate::days(1), max(result$date, lubridate::today(tz="UTC"))  + lubridate::days(7), by="day"),
                                usd_per_bbl = result %>% arrange(date) %>% tail(7) %>% pull(usd_per_bbl) %>% mean())

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


get_espo_brent_spread <- function(){

  date_to <- lubridate::today() + lubridate::days(7)
  get_at_date <- function(target_date){
    espo_discount = tibble(date=c('1990-01-01', '2022-02-24', '2022-03-11', '2022-03-17',
                                  '2022-04-11', '2022-05-04', as.character(date_to)),
                           discount=c(0,0,0,20,26,30,30))
    approx(as_date(espo_discount$date), espo_discount$discount, as_date(target_date))$y
  }
  dates <- seq.Date(as.Date("2022-01-01"), date_to, by="day")
  # eur_per_usd <- price.eur_per_usd(date_from=min(dates), date_to=max(dates))
  # tonne_per_bbl <- 0.138
  tibble(date=dates, usd_per_bbl=-get_at_date(date)) %>%
    select(date, usd_per_bbl) %>%
    arrange(desc(date))


    # left_join(eur_per_usd) %>%
    # mutate(reduction_eur_per_tonne=-values_usd_per_bbl * eur_per_usd / tonne_per_bbl,
    #        commodity="crude_oil") %>%
    # select(commodity, date, reduction_eur_per_tonne)
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
