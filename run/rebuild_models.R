options(readr.show_col_types = FALSE)
library(russiacounter)
library(argparse)

library(logger)

log_layout(layout_simple)
log_formatter(formatter_glue)
log_threshold(DEBUG)
log_appender(appender_console)


tryCatch(
  {
    russiacounter::price_models_eurostat.build()

    russiacounter::price_models_comtrade.build()
  },
  error = function(e) {
    tryCatch(rlang::last_trace(), error = function(err) NULL)
    log_error("Error building price models: {e$message}")
    stop(e)
  }
)

log_info("Finished rebuilding price models")
