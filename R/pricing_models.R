require(tidyverse); require(magrittr)

ttf <- quantmod::getSymbols("TTF=F",
                            from = '2016-01-01',
                            warnings = FALSE,
                            auto.assign = F)

ttf %>% as_tibble %>% mutate(date = ttf %>% as.data.frame %>% row.names %>% ymd) %>%
  rename(TTF = contains('Close')) %>% group_by(date = date %>% 'day<-'(1)) %>%
  summarise(across(TTF, mean, na.rm=T)) -> ttf_monthly

brent <- tidyquant::tq_get("BZ=F", from='2016-01-01')

brent %>% rename(Brent = close) %>% group_by(date = date %>% 'day<-'(1)) %>%
  summarise(across(Brent, mean, na.rm=T)) -> brent_monthly

#data downloaded from https://www.investing.com/commodities/rotterdam-coal-futures-streaming-chart
ara <- read_csv('data/Rotterdam_Coal_Futures_Historical_Data.csv')

ara %>% rename(ARA = Price) %>%
  group_by(date = Date %>% mdy() %>% 'day<-'(1)) %>%
  summarise(across(ARA, mean, na.rm=T)) -> ara_monthly

prices_monthly <- ttf_monthly %>% full_join(brent_monthly) %>% full_join(ara_monthly)

#data downloaded from https://appsso.eurostat.ec.europa.eu/nui/show.do?query=BOOKMARK_DS-1262527_QID_4012F26E_UID_-3F171EB0&layout=PERIOD,L,X,0;INDICATORS,C,Y,0;TRANSPORT_MODE,L,Y,1;PARTNER,L,Z,0;FLOW,L,Z,1;REPORTER,L,Z,2;PRODUCT,L,Z,3;&zSelection=DS-1262527REPORTER,EU;DS-1262527FLOW,1;DS-1262527PRODUCT,2701;DS-1262527PARTNER,RU;&rankName1=PARTNER_1_2_-1_2&rankName2=FLOW_1_2_-1_2&rankName3=REPORTER_1_2_1_1&rankName4=PRODUCT_1_2_0_1&rankName5=PERIOD_1_0_0_0&rankName6=INDICATORS_1_2_0_1&rankName7=TRANSPORT-MODE_1_2_1_1&sortC=ASC_-1_FIRST&rStp=&cStp=&rDCh=&cDCh=&rDM=true&cDM=true&footnes=false&empty=true&wai=false&time_mode=ROLLING&time_most_recent=false&lang=EN&cfo=%23%23%23%2C%23%23%23.%23%23%23
trade <- read_csv('data/DS-1262527_1_Data.csv')



trade %<>% mutate(date=paste(1,PERIOD) %>% dmy(),
                 Value = gsub(',', '', Value) %>% as.numeric) %>%
  spread(INDICATORS, Value) %>%
  rename(t = QUANTITY_IN_TONS, EUR = VALUE_IN_EUROS) %>%
  mutate(price = EUR/t)

trade$PRODUCT[grep('crude$', trade$PRODUCT)] <- "crude oil"
trade$PRODUCT[grep('excl\\. crude', trade$PRODUCT)] <- "oil products"
trade$PRODUCT[grep('^Coal', trade$PRODUCT)] <- "coal"
trade$PRODUCT[grep('gas', trade$PRODUCT)] <- "gas"
trade$TRANSPORT_MODE[grep('Fixed', trade$TRANSPORT_MODE)] <- "Pipeline"

trade %<>% filter(grepl('Pipeline|Sea', TRANSPORT_MODE),
                  grepl('^gas|^coal|crude oil|oil products', PRODUCT),
                  !is.na(price)) %>%
  left_join(prices_monthly)

trade %>% group_by(PRODUCT, TRANSPORT_MODE) %>%
  group_map(function(df, group) {
    start_year = ifelse(group$PRODUCT=='gas' & group$TRANSPORT_MODE == 'Pipeline', 2015, 2015)
    independents = case_when(group$PRODUCT=='coal' ~ 'ARA',
                             group$PRODUCT=='gas' & group$TRANSPORT_MODE == 'Pipeline' ~
                               'Brent+TTF*date*abs(month(date)-9)',
                             group$PRODUCT=='gas' ~ 'TTF',
                             grepl('oil', group$PRODUCT) & group$TRANSPORT_MODE == 'Sea' ~
                               'Brent + lag(Brent) + lag(Brent, 3) + 0',
                             T ~ 'Brent + lag(price, 12)')

      df %>% arrange(date) %>% filter(year(date)>=start_year) %>%
      lm(as.formula(paste('price ~', independents)), data=.) -> m

    list(data=df %>% mutate(predicted_price = predict(m, df)),
         model=m,
         group=group)
  }) -> trade_with_predictions

trade_with_predictions %>% lapply(function(x) bind_cols(x$group, x$data)) %>%
  bind_rows() %>% pivot_longer(matches('price')) %>% filter(year(date)>=2016) %>%
  ggplot(aes(date, value, col=name)) + facet_grid(TRANSPORT_MODE~PRODUCT) + geom_line()

trade_with_predictions %>% lapply(function(x) bind_cols(x$group, x$data)) %>%
  bind_rows() %>% filter(year(date)>=2016) %>%
  ggplot(aes(price, predicted_price, col=TRANSPORT_MODE)) +
  facet_wrap(~PRODUCT, scales='free') + geom_point() + geom_abline()+ geom_smooth()

saveRDS(trade_with_predictions, 'data/pricing_models_and_data.RDS')
