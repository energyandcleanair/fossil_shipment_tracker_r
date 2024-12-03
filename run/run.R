options(readr.show_col_types = FALSE)
library(russiacounter)
library(argparse)

library(logger)

log_layout(layout_simple)
log_formatter(formatter_glue)
log_threshold(DEBUG)
log_appender(appender_console)

parser <- ArgumentParser()
parser$add_argument("--rebuild_prices", type = "logical", default = FALSE)
args <- parser$parse_args()

russiacounter::update_counter(rebuild_prices = args$rebuild_prices)
