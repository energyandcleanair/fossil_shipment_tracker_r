get_comtrade <- function(){
  require(tidyverse); require(magrittr); require(countrycode)
  library(comtradr)
  library(tidyquant)


  get_year_data <- function(year){

    # usd_per_eur <- getSymbols("EUR=X", from=paste0(year,"-01-01"), to= paste0(year,"-12-31"), auto.assign = F)
    # usd_per_eur <- tibble(date=index(usd_per_eur), value=as.numeric(usd_per_eur$`EUR=X.Adjusted`)) %>%
    #   group_by(date=lubridate::floor_date(date, 'month')) %>%
    #   summarise(usd_per_eur=mean(value, na.rm=T))

    # oil_codes <- c("2709","2710")
    gas_codes <- c("271121")
    # coal_codes <- c("2701","2704","2705","2706")

    q_import <- ct_search(partners = "Russian Federation",
              reporters = c("Germany", "Poland", "Finland", "Ukraine", "Estonia"), #TODO add countries
              trade_direction = "imports",
              commod_codes=gas_codes, #c(oil_codes, gas_codes, coal_codes),
              freq="monthly",
              start_date = year,
              end_date = year)

    q_export <- ct_search(partners = "Russian Federation",
                          reporters = c("Germany", "Poland", "Finland", "Ukraine", "Estonia"), #TODO add countries
                          trade_direction = "exports",
                          commod_codes=c(oil_codes, gas_codes, coal_codes),
                          freq="monthly",
                          start_date = year,
                          end_date = year) %>%
      mutate(netweight_kg=-netweight_kg,
             trade_value_usd=-trade_value_usd)

    q <- bind_rows(q_import, q_export) %>%
      mutate(date=as.Date(strptime(paste0(period,"01"), format="%Y%m%d"))) %>%
      group_by(commodity_code, commodity, date, reporter, partner) %>%
      summarise_at(c("netweight_kg", "trade_value_usd"), sum, na.rm=T) %>%
      mutate(value=netweight_kg / kg_per_m3 * gcv_MWh_per_m3,
             unit="MWh/month") %>%
      # left_join(usd_per_eur) %>%
      # mutate(value_EUR=trade_value_usd/usd_per_eur) %>%
      select(-c(trade_value_usd, netweight_kg)) %>%
      select(country=reporter, date, commodity, value, unit)

    if(nrow(q)==0){
      return(NULL)
    }else{
      return(q)
    }
  }

  q <- lapply(seq(2017, 2022), get_year_data) %>% do.call(bind_rows, .)
  return(q)
}




