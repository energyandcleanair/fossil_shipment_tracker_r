library(tidyverse)
source('entsog.R')

operators <- entsog.operators()

operators_russia <- operators %>%
  filter(operatorCountryLabel=="Russia")

interconnections_russia <-
  entsog.interconnections(from_operator_key = operators_russia$operatorKey)

flow_russia <- entsog.physical_flows(
  operator_key = interconnections_russia$toOperatorKey,
  point_key = interconnections_russia$toPointKey,
  direction = "entry",
  date_from="2017-01-01")


flow_russia %>%
  group_by(flowStatus, periodFrom, operatorLabel) %>%
  summarise_at("value", sum, na.rm=T) %>%
  ggplot() +
  geom_area(aes(periodFrom, value, fill=operatorLabel))
