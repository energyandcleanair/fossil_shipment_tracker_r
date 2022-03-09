price.ttf <- function(date_from="2016-01-01"){
  p <- quantmod::getSymbols("TTF=F",
                  from = date_from,
                  warnings = FALSE,
                  auto.assign = F)
  tibble(date=index(p),
         price=as.numeric(coredata(p$`TTF=F.Adjusted`)[]),
         unit="EUR/MWh")
}


price.brent<- function(date_from="2020-01-01"){
    p <- tidyquant::tq_get("BZ=F", from=date_from)
    p %>%
    select(date, value=adjusted) %>%
      mutate(unit="USD/bbl") %>%
      drop_na()
}


price.eur_per_usd <- function(date_from="2018-01-01"){
  eur_per_usd <- quantmod::getSymbols("EUR=X", from=date_from, auto.assign = F)
  tibble(date=zoo::index(eur_per_usd), eur_per_usd=as.numeric(eur_per_usd$`EUR=X.Adjusted`))
}


price.natural_gas_based_on_brent<- function(date_from="2018-01-01"){
  # oil price of $69/barrel = $280 / 1000 m3 for gas; contracts are tied to oil
  # https://www.reuters.com/markets/europe/russias-oil-gas-revenue-windfall-2022-01-21/
  eur_per_usd <- price.eur_per_usd(date_from=date_from)

  tidyquant::tq_get("BZ=F", from=date_from) %>%
    select(date, value=adjusted) %>% #USD/bbl
    left_join(eur_per_usd) %>%
    mutate(value=value* 280 / 69 /1000 / gcv_MWh_per_m3 * eur_per_usd) %>%
    mutate(unit="EUR/MWh") %>%
    select(-c(eur_per_usd)) %>%
    rcrea::utils.running_average(30) %>%
    mutate(commodity="natural_gas")

}


#' Price based on Comtrade data, which is given both in kg and USD
#'
#' @return
#' @export
#'
#' @examples
price.comtrade <- function(){
  flows <- comtrade.get_flows(use_cache=F)

  flows %>%
    filter(unit=="MWh/month") %>%
    mutate(value_usd_per_mwh=value_usd/value) %>%
    ggplot() +
    geom_line(aes(date, value_usd_per_mwh, col=commodity)) +
    facet_wrap(~country, scales="free")
}


price.get_modelled_price <- function(flows_entsog, flows_eurostat_exeu){

  flows <- bind_rows(
    flows_entsog %>%
      filter(unit=="MWh/day") %>%
      mutate(value=value/gcv_MWh_per_m3*kg_per_m3/1000,
             unit="tonne",
             transport="pipeline") %>%
      group_by(date, commodity, transport, unit, source) %>%
      summarise(value=sum(value)) %>%
      mutate(country="All"),

    # Spread on a daily basis as opposed to a monthly one
    flows_eurostat_exeu %>%
      filter(unit=="tonne") %>%
      filter(paste(commodity, transport) != paste("natural_gas", "pipeline")) %>%
      right_join(
        tibble(date_day=seq(min(flows_eurostat_exeu$date),
                            lubridate::ceiling_date(max(flows_eurostat_exeu$date), "month") - 1, by="day")) %>%
          mutate(date=lubridate::floor_date(date_day, 'month'))
      ) %>%
      mutate(value=value / lubridate::days_in_month(date)) %>%
      mutate(date=date_day) %>%
      select(-c(date_day))
  ) %>%
    mutate(month=lubridate::floor_date(date, "month"))

  # Get pricing
  flows_month <- flows %>%
    distinct(date=month, transport, commodity, price)

  # models <- readRDS('data/pricing_models.RDS')
  models <- readRDS(system.file("extdata", "pricing_models.RDS", package="russiacounter"))

  # Get predictors
  ttf <- quantmod::getSymbols("TTF=F", from = '2016-01-01', warnings = FALSE, auto.assign = F)
  ttf_monthly <- ttf %>% as_tibble %>% mutate(date = ttf %>% as.data.frame %>% row.names %>% ymd) %>%
    rename(TTF = contains('Close')) %>% group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(TTF, mean, na.rm=T))

  brent <- tidyquant::tq_get("BZ=F", from='2016-01-01')
  brent_monthly <- brent %>% rename(Brent = close) %>% group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(Brent, mean, na.rm=T))

  #data downloaded from https://www.investing.com/commodities/rotterdam-coal-futures-streaming-chart
  ara <- read_csv('data/Rotterdam_Coal_Futures_Historical_Data.csv')
  ara_monthly <- ara %>% rename(ARA = Price) %>%
    group_by(date = Date %>% mdy() %>% 'day<-'(1)) %>%
    summarise(across(ARA, mean, na.rm=T))

  prices <- flows_month %>%
    filter(date >= '2016-01-01') %>%
    left_join(ttf_monthly) %>%
    left_join(brent_monthly) %>%
    left_join(ara_monthly) %>%
    group_by(commodity, transport) %>%
    tidyr::nest() %>%
    left_join(models %>% select(-c(data)), by=c("commodity", "transport")) %>%
    group_by(commodity, transport) %>%
    group_map(function(df, group) {

      start_year <- 2015
      model <- df$model[[1]]
      data <- df$data[[1]] %>% arrange(date)
      if(is.null(model)){ return(NULL) }
      data$price <- predict(model, data)
      tibble(group, data)
      }) %>% do.call(bind_rows, .)


  # Combine
  flows %>%
    select(-c(price)) %>%
    left_join(prices %>% rename(month=date)) %>%
    mutate(value_eur=value * price)
}
