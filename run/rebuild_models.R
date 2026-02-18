options(readr.show_col_types = FALSE)
library(russiacounter)
library(argparse)

library(logger)

log_layout(layout_simple)
log_formatter(formatter_glue)
log_threshold(DEBUG)
log_appender(appender_console)

russiacounter::price_models_eurostat.build(
    production = T
)

russiacounter::price_models_comtrade.build(
    production = T
)
