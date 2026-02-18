options(readr.show_col_types = FALSE)
library(russiacounter)
library(argparse)
library(tidyverse)

library(logger)

log_layout(layout_simple)
log_formatter(formatter_glue)
log_threshold(DEBUG)
log_appender(appender_console)
devtools::load_all(".")

log_level(STAGE, "Updating european pipeline gas")
flows_entsog <- entsog_new.get_flows(
    date_from = "2025-07-14",
    date_to = lubridate::today() - 4,
    use_cache = F
) %>%
    mutate(
        process = "gas_model"
    )
ok <- (
    sum(
        flows_entsog$value_tonne
    ) >= as.integer(
        max(flows_entsog$date) - min(flows_entsog$date)
    ) * 5e5
)
if (ok) {
    db.upload_flows_to_postgres(flows_entsog)
}
