library(tidyverse)


update_flows(source="eurostat_exeu")



prices <- price.get_modelled_price(flows_entsog=entsog.get_flows(),
                                   flows_eurostat_exeu=eurostat_exeu.get_flows())
db.upload_flows(flows=prices, source="combined")


# seq(2021, 2016) %>% lapply(creapower::update_generation, data_source="entso")
#
# seq(2021, 2016) %>% lapply(creapower::update_generation, data_source="eia")
#
# seq(2021, 2016) %>% lapply(creapower::update_generation, data_source="posoco")
#
# seq(2021, 2016) %>% lapply(creapower::update_generation, data_source="bmrs")
