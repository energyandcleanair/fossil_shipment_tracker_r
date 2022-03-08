entsog.get_flows <- function(use_cache=T){

  f <- file.path("cache/entsog.RDS")
  dir.create("cache", F)

  if(use_cache && file.exists(f)){ return(readRDS(f)) }

  operators <- entsog::operators()
  interconnections <- entsog::interconnections()

#   operators_russia <- operators %>%
#     filter(operatorCountryLabel=="Russia")

  # Russia exports
  interconnections_russia_exports <-
    interconnections %>% filter(fromCountryKey %in% c("RU", "BY"))

  flows_russia_exports <- entsog::physical_flows(
    operator_key = interconnections_russia_exports$toOperatorKey,
    point_key = interconnections_russia_exports$toPointKey,
    direction = "entry",
    date_from="2022-01-01") %>%
    left_join(
      tibble(pointKey=interconnections_russia_exports$toPointKey,
             country=interconnections_russia_exports$toCountryLabel,
             partner=interconnections_russia_exports$fromCountryLabel)
    )


  # Russia imports
  interconnections_russia_imports <-
    interconnections %>% filter(toCountryKey %in% c("RU", "BY"))

  flows_russia_imports <- entsog::physical_flows(
    operator_key = interconnections_russia_imports$fromOperatorKey[1],
    point_key = interconnections_russia_imports$fromPointKey[1],
    direction = "exit",
    date_from="2022-01-01")

  flows_russia_imports <- flows_russia_imports %>%
    left_join(
      tibble(pointKey=interconnections_russia_imports$fromPointKey,
             country=interconnections_russia_imports$fromCountryLabel,
             partner=interconnections_russia_imports$toCountryLabel)
    )

  flows_russia_imports$value <- flows_russia_imports$value * -1

  flows_russia <- bind_rows(
    flows_russia_exports,
    flows_russia_imports
  ) %>%
    filter(unit=="kWh/d") %>%
    group_by(date=as.Date(date), country, partner) %>%
    summarise(value=sum(value, na.rm=T)/1000) %>%
    mutate(unit="MWh/day",
           commodity="natural_gas",
           source="entsog") %>%
    ungroup()


  # Cut tail with insufficient values
  count <- flows_russia %>% group_by(date) %>% summarise(count=n())
  thresholds <- count %>% arrange(desc(date)) %>% head(10) %>% summarise(count=mean(count), date=min(date))
  flows_russia <-  flows_russia %>%
    left_join(count) %>%
    filter((date<=thresholds$date) | (count >= thresholds$count)) %>% arrange(desc(date))


  saveRDS(flows_russia, f)
  return(flows_russia)
}


