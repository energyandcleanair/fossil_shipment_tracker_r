get_entsog <- function(use_cache=T){

  f <- file.path("cache/russia_gas_exports.RDS")
  dir.create("cache", F)

  if(use_cache && file.exists(f)){ return(readRDS(f)) }

  operators <- entsog::operators()
  # zones <- entsog::balancing_zones()
  interconnections <- entsog::interconnections()

  operators_russia <- operators %>%
    filter(operatorCountryLabel=="Russia")

  # Russia exports
  interconnections_russia <-
    entsog::interconnections(from_operator_key = operators_russia$operatorKey)

  flows_russia_exports <- entsog::physical_flows(
    operator_key = interconnections_russia$toOperatorKey,
    point_key = interconnections_russia$toPointKey,
    direction = "entry",
    date_from="2017-01-01")


  # Russia imports
  interconnections_russia_imports <-
    entsog::interconnections(to_operator_key = operators_russia$operatorKey)

  flows_russia_imports <- entsog::physical_flows(
    operator_key = interconnections_russia_imports$fromOperatorKey,
    point_key = interconnections_russia_imports$fromPointKey,
    direction = "exit",
    date_from="2017-01-01")

  flows_russia_imports$value <- flows_russia_imports$value * -1

  flows_russia <- bind_rows(
    flows_russia_exports,
    flows_russia_imports
  )

  flows_russia <- flows_russia %>%
    filter(unit=="kWh/d") %>%
    left_join(operators %>% distinct(operatorKey, operatorCountryLabel)) %>%
    group_by(date=as.Date(date), country=operatorCountryLabel) %>%
    summarise(value=sum(value, na.rm=T)/1000) %>%
    mutate(unit="MWh/day",
           commodity="Natural Gas",
           source="ENTSOG")
    ungroup()

  saveRDS(flows_russia, f)
  return(flows_russia)
}


