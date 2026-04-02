options(readr.show_col_types = FALSE)
library(russiacounter)
library(argparse)

library(logger)

log_layout(layout_simple)
log_formatter(formatter_glue)
log_threshold(DEBUG)
log_appender(appender_console)

parser <- ArgumentParser()
args <- parser$parse_args()

russiacounter::price.update_prices(buffer_days = 60, rebuild = TRUE)
