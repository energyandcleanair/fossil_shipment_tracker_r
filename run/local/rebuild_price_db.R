options(readr.show_col_types = FALSE)
devtools::load_all(".")
library(argparse)

library(logger)

log_layout(layout_simple)
log_formatter(formatter_glue)
log_threshold(DEBUG)
log_appender(appender_console)

price.update_prices(buffer_days = 60)
