china.get_flows <- function(){
  #China pipeline oil imports 41 Mtpa

  bind_rows(
   china.get_flows_natural_gas(),
   china.get_flows_pipeline_oil()
   )
}

china.get_flows_natural_gas <- function(){

  #China pipeline gas 3.9 bcm in 2020; 5 bcm over 161 days in 2021-22 - assume 10 bcmpa
  #http://www.xinhuanet.com/english/2021-08/10/c_1310119621.htm
  #https://news.cgtn.com/news/2022-01-18/China-Russia-pipeline-delivers-15b-cubic-meters-of-natural-gas--16V6fC0jWQo/index.html

    tibble(commodity='natural_gas',
           value_m3=c(10e9/365),
           value_tonne=c(10e9/1000/365*0.7168),
    ) %>%
      tidyr::crossing(tibble(date=seq.Date(as.Date("2021-01-01"), lubridate::today(), by="day"))) %>%
      mutate(departure_iso2='RU',
             destination_iso2='CN')
}



china.get_flows_pipeline_oil_old <- function(){

  # China is set to receive approximately 880,000 bpd of Russian oil via the two East Siberia-Pacific Ocean Pipelines (ESPO) and the Kazakhstan-China pipeline under government deals
  # https://www.reuters.com/business/energy/china-extends-record-imports-russian-oil-into-june-cuts-saudi-supply-trade-2022-07-06/

  # Total - shipments -> Pipeline
  #
  # tonne_per_bl = 0.136
  # old <- tibble(
  #   value_m3=c(NA,NA,NA),
  #   value_tonne=c(41e6/365, 880e3*tonne_per_bl, 880e3*tonne_per_bl),
  #   date=c(as.Date("2022-01-01"), as.Date("2022-06-01"), lubridate::today())
  # ) %>%
  #   full_join(tibble(date=seq.Date(as.Date("2022-01-01"), lubridate::today(), by="day"),
  #                    partner='China',
  #                    commodity=c('pipeline_oil'))) %>%
  #   arrange(date) %>%
  #   fill(value_tonne) %>%
  #   mutate(departure_iso2='RU',
  #          destination_iso2='CN')
  # )

}


china.get_flows_pipeline_oil <- function(){

  # Getting total monthly data from China customs: http://43.248.49.97/indexEn
  # Or from wind
  # And deducing shipments

  # Capping: "The estimated ESPO oil pipeline capacity for deliveries to mainland China for Russian oil is approximately 35 MMt/y"
  # max_value_tonne = 35e6/365

  shipments <- read_csv('https://api.russiafossiltracker.com/v0/voyage?date_from=2022-01-01&commodity=crude_oil&aggregate_by=arrival_month,commodity&commodity_destination_iso2=CN&commodity_origin_iso2=RU&format=csv&bypass_maintenance=true') %>%
    select(month=arrival_month, value_shipment_tonne_month=value_tonne)

  total <- read_csv(system.file("extdata","china/china_imports_wind.csv", package="russiacounter"), skip=1) %>%
    select(date=Name,
           value_kg=`Volume of Imports: Crude Oil: Russian Federation`) %>%
    filter(grepl('^20', date)) %>%
    mutate(
      # month=as.Date(paste0(date,'-01')),
          month=floor_date(as.Date(date), 'month'),
           value_total_tonne_month=as.numeric(gsub(",", "", value_kg))/1000) %>%
    select(month, value_total_tonne_month)

  pipeline <- shipments %>%
    left_join(total) %>%
    filter(!is.na(value_total_tonne_month)) %>%
    full_join(
      tibble(date=seq.Date(as.Date('2022-01-01'), lubridate::today() + lubridate::days(31), by='day')) %>%
        mutate(month=lubridate::floor_date(date, 'month'),
               ndays=lubridate::days_in_month(month))
    ) %>%
    mutate(value_tonne=(value_total_tonne_month - value_shipment_tonne_month)/ndays) %>%
    # Fill missing days
    arrange(date) %>%
    tidyr::fill(value_tonne) %>%
    select(date, value_tonne) %>%
    mutate(departure_iso2='RU',
           destination_iso2='CN',
           commodity='pipeline_oil',
           value_m3=NA_real_) %>%
    select(departure_iso2, destination_iso2, commodity, date, value_tonne, value_m3)

  return(pipeline)
}

china.explore <- function(){
  # So I took data from China customs here: http://43.248.49.97/indexEn
  # It seems like this site/filtering gives pipeline data directly: red is shipment, blue is our pipeline assumption, green is China customs for crude imports from Russia ('Petroleum oils&oils obtained from bituminous minerals, crude'). The numbers are those that have been mentioned by media for ESPO (e.g. the 8.42 mn tonnes here for May).
  # On China customs stats page, I can't find product x origin table... but I must miss something since all medias quoted a number... Anyway, can talk about it tomorrow. Total imports are more on the 30-40 million tonnes monthly, roughly twice our Russia -> China values, which makes sense. (edited)


  # # Check how much shipment we had and deduct
  pipeline <- read_csv('https://api.russiafossiltracker.com/v0/counter?date_from=2022-01-01&commodity=pipeline_oil&aggregate_by=month,commodity&destination_iso2=CN&format=csv')

  direct_shipment <- read_csv('https://api.russiafossiltracker.com/v0/voyage?date_from=2022-01-01&commodity=crude_oil&aggregate_by=arrival_month,commodity&destination_iso2=CN&commodity_origin_iso2=RU&format=csv') %>%
    # filter(destination_iso2!='CN') %>%
    mutate(commodity='crude_oil directly')


  existing_kr_to_cn <- read_csv('https://api.russiafossiltracker.com/v0/voyage?date_from=2022-01-01&commodity=crude_oil&aggregate_by=arrival_month,commodity&destination_iso2=KR&commodity_destination_iso2=CN&format=csv') %>%
    # filter(destination_iso2!='CN') %>%
    mutate(commodity='crude_oil through other country')

  all_shipments <- read_csv('https://api.russiafossiltracker.com/v0/voyage?date_from=2022-01-01&commodity=crude_oil&aggregate_by=arrival_month,commodity&commodity_origin_iso2=RU&commodity_destination_iso2=CN&format=csv') %>%
    # filter(destination_iso2!='CN') %>%
    mutate(commodity='crude_oil')

  counter_cn <- read_csv('https://api.russiafossiltracker.com/v0/counter?date_from=2022-01-01&commodity=crude_oil,pipeline_oil&aggregate_by=month,commodity&destination_iso2=CN&format=csv')


  ggplot(bind_rows(
    counter_cn %>%
      select(month, commodity, value_tonne) %>%
      mutate(source='Counter'),
    pipeline %>%
      select(month, commodity, value_tonne) %>%
      mutate(source='CREA'),
    all_shipments%>%
      select(month=arrival_month, commodity, value_tonne) %>%
      mutate(source='CREA'),
    # direct_shipment %>%
    #   select(month=arrival_month, commodity, value_tonne) %>%
    #   mutate(source='CREA'),
    # existing_kr_to_cn %>%
    #   select(month=arrival_month, commodity, value_tonne) %>%
    #   mutate(source='CREA')
    ) %>%
      filter(month<='2022-07-01')) +
    geom_area(aes(month, value_tonne, fill=commodity)) +
    facet_wrap(~source)

}
