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
