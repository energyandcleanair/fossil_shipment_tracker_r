library(testthat)


test_that('entsog_attribution',{

  get_consumption <- function(flows){
    flows %>% group_by(country=from_country) %>% summarise(export=sum(value, na.rm=T)) %>%
      full_join(flows %>% group_by(country=to_country) %>% summarise(import=sum(value, na.rm=T)),
                by=c("country")) %>%
      mutate(consumption=tidyr::replace_na(import, 0) - tidyr::replace_na(export, 0)) %>%
      arrange(country) %>%
      select(country, consumption) %>%
      filter(consumption != 0) %>%
      filter(!country %in% c("Russia", "Ukraine"))
  }

  flows <- tibble(
    from_country=c("1","2","4","2"),
    to_country=c("2","3","2","5"),
    value=c(100,15,50,30)
  )

  flows_sourced <- process_iterative(flows) %>%
    filter(value!=0) %>%
    select(from_country, to_country, value)

  consumption_before <- get_consumption(flows) %>% arrange(country)
  consumption_after <- get_consumption(flows_sourced) %>% arrange(country)

  expect_equal(consumption_before[c('country','consumption')], consumption_after[c('country','consumption')])


  # Test with entsog data
  flows <- read_csv("../../fossil_shipment_tracker/entsog_flows_False_False_False_True.csv") %>%
    filter(date>=date_from) %>%
    mutate(value=value/gcv_kWh_per_m3/1e9) %>% #value is now in bcm
    select(-c(index))

  flows_2020 <- flows %>%
    filter(lubridate::year(date)==2020)

  consumption_2020 <- get_consumption(flows_2020) %>% arrange(desc(consumption))

  a <- pbapply::pblapply(split(flows, flows$date), function(flows_before){

    flows_after <- process_iterative(flows_before)
    consumption_before <- get_consumption(flows_before)
    consumption_after <- get_consumption(flows_after)

    expect_equal(consumption_before[c('country','consumption')], consumption_after[c('country','consumption')])
  })



})
