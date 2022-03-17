build_price_models <- function(flows_comtrade_eurostat){

  library(tidyverse)
  library(magrittr)
  library(lubridate)

  # Collect predictors
  ttf <- quantmod::getSymbols("TTF=F", from = '2016-01-01', warnings = FALSE, auto.assign = F)
  ttf_monthly <- ttf %>% as_tibble %>% mutate(date = ttf %>% as.data.frame %>% row.names %>% ymd) %>%
    rename(TTF = contains('Close')) %>% group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(TTF, mean, na.rm=T))

  brent <- tidyquant::tq_get("BZ=F", from='2016-01-01')
  brent_monthly <- brent %>% rename(Brent = close) %>% group_by(date = date %>% 'day<-'(1)) %>%
    summarise(across(Brent, mean, na.rm=T))

  #data downloaded from https://www.investing.com/commodities/rotterdam-coal-futures-streaming-chart
  ara <- readr::read_csv(system.file("extdata", "Rotterdam_Coal_Futures_Historical_Data.csv", package="russiacounter"))
  ara_monthly <- ara %>% rename(ARA = Price) %>%
    group_by(date = Date %>% mdy() %>% 'day<-'(1)) %>%
    summarise(across(ARA, mean, na.rm=T))

  prices_monthly <- ttf_monthly %>% full_join(brent_monthly) %>% full_join(ara_monthly)

  #data downloaded from https://appsso.eurostat.ec.europa.eu/nui/show.do?query=BOOKMARK_DS-1262527_QID_4012F26E_UID_-3F171EB0&layout=PERIOD,L,X,0;INDICATORS,C,Y,0;TRANSPORT_MODE,L,Y,1;PARTNER,L,Z,0;FLOW,L,Z,1;REPORTER,L,Z,2;PRODUCT,L,Z,3;&zSelection=DS-1262527REPORTER,EU;DS-1262527FLOW,1;DS-1262527PRODUCT,2701;DS-1262527PARTNER,RU;&rankName1=PARTNER_1_2_-1_2&rankName2=FLOW_1_2_-1_2&rankName3=REPORTER_1_2_1_1&rankName4=PRODUCT_1_2_0_1&rankName5=PERIOD_1_0_0_0&rankName6=INDICATORS_1_2_0_1&rankName7=TRANSPORT-MODE_1_2_1_1&sortC=ASC_-1_FIRST&rStp=&cStp=&rDCh=&cDCh=&rDM=true&cDM=true&footnes=false&empty=true&wai=false&time_mode=ROLLING&time_most_recent=false&lang=EN&cfo=%23%23%23%2C%23%23%23.%23%23%23
  # trade <- read_csv('data/DS-1262527_1_Data.csv') %>%
  #   mutate(date=paste(1,PERIOD) %>% strptime("%d %b. %Y") %>% as.Date(),
  #          Value = gsub(',', '', Value) %>% as.numeric) %>%
  #   filter(!is.na(date)) %>%
  #   spread(INDICATORS, Value) %>%
  #   select(date, country=REPORTER, partner=PARTNER, direction=FLOW, commodity=PRODUCT, transport=TRANSPORT_MODE,
  #          t = QUANTITY_IN_TONS, EUR = VALUE_IN_EUROS) %>%
  #   mutate(price = EUR/t)
#
#
#   trade$commodity[grep('crude$', trade$commodity)] <- "crude_oil"
#   trade$commodity[grep('excl\\. crude', trade$commodity)] <- "oil_products"
#   trade$commodity[grep('^Coal', trade$commodity)] <- "coal"
#   # trade$commodity[grep('gas', trade$commodity)] <- "natural_gas"
#   trade$commodity[grepl('gas', trade$commodity) & grepl('Fixed', trade$transport)] <- "natural_gas"
#   trade$commodity[grepl('gas', trade$commodity) & grepl('Sea', trade$transport)] <- "lng"
#
#
#   trade$transport[grep('Fixed', trade$transport)] <- "pipeline"
#   trade$transport[grep('Sea', trade$transport)] <- "sea"

  # trade %<>% filter(grepl('pipeline|sea', transport),
  #                   grepl('^natural_gas|lng|^coal|crude_oil|oil_products', commodity),
  #                   !is.na(price)) %>%
  #   left_join(prices_monthly)

  trade <- flows_comtrade_eurostat %>%
    filter(EU) %>%
    filter( grepl('^natural_gas|lng|^coal|oil|oil_others', commodity)) %>%
    tidyr::spread(unit, value) %>%
    mutate(price=abs(eur/tonne)) %>%
    filter((commodity!="lng") | (price<1500)) %>%
    filter(!is.na(price) & !is.infinite(price)) %>%
    filter(tonne>100) %>%
    group_by(date, commodity) %>%
    summarise_at(c("eur","tonne"), sum, na.rm=T) %>%
    mutate(price=abs(eur/tonne)) %>%
    filter(!is.na(price) & !is.infinite(price)) %>%
    left_join(prices_monthly)

  trade %>% group_by(commodity) %>%
    group_map(function(df, group) {
      print(group$commodity)
      start_year = ifelse(group$commodity=='natural_gas', 2015, 2015)
      independents = case_when(group$commodity=='coal' ~ 'ARA',
                               group$commodity=='natural_gas' ~
                                 'Brent+TTF*date*abs(month(date)-9)',
                               group$commodity=='lng' ~ 'TTF',
                               grepl('oil', group$commodity) ~
                                 'Brent + lag(Brent) + lag(Brent, 3) + 0',
                               T ~ 'Brent + lag(price, 12)')

        df %>% arrange(date) %>% filter(year(date)>=start_year) %>%
        lm(as.formula(paste('price ~', independents)), data=.) -> m

      tibble_row(data=list(as.data.frame(df %>% mutate(predicted_price = predict(m, df)))),
           model=list(m),
           commodity=group$commodity)
    }) %>% do.call(bind_rows, .)-> trade_with_predictions

  trade_with_predictions %>% select(-c(model)) %>%
    tidyr::unnest(data)  %>% pivot_longer(matches('price')) %>% filter(year(date)>=2016) %>%
    ggplot(aes(date, value, col=name)) + facet_grid(~commodity) + geom_line()

  trade_with_predictions %>% select(-c(model)) %>%
    tidyr::unnest(data)  %>% filter(year(date)>=2016) %>%
    ggplot(aes(price, predicted_price)) +
    facet_wrap(~commodity, scales='free') + geom_point() + geom_abline()+ geom_smooth()

  saveRDS(trade_with_predictions, 'inst/extdata/pricing_models.RDS')
  saveRDS(trade_with_predictions, 'data/pricing_models.RDS')
}
