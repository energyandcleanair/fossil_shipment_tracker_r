update_counter <- function(rebuild_prices = F) {
  suppressPackageStartupMessages(library(tidyverse))
  suppressPackageStartupMessages(library(lubridate))
  suppressPackageStartupMessages(library(magrittr))
  suppressPackageStartupMessages(library(countrycode))
  suppressPackageStartupMessages(library(rcrea))
  suppressPackageStartupMessages(library(pbapply))
  suppressPackageStartupMessages(library(eurostat))
  suppressPackageStartupMessages(library(forecast))

  log_info("Building stages")

  stages <- list(
    list(name = "Updating prices", code = function() {
      price.update_prices(production = T, buffer_days = 60, rebuild = rebuild_prices)
    }),
    list(name = "Updating european pipeline gas", code = function() {
      flows_entsog <- entsog_new.get_flows(
        date_from = lubridate::today() - 21,
        date_to = lubridate::today() - 2,
        predict_future_days = 7,
        use_cache = FALSE
      ) %>%
        mutate(
          process = "gas_model"
        )
      ok <- T & (sum(flows_entsog$value_tonne) >= as.integer(max(flows_entsog$date) - min(flows_entsog$date)) * 5e5)
      if (ok) {
        db.upload_flows_to_postgres(flows_entsog, production = T)
      }
    }),
    list(name = "Updating other european overland flows", code = function() {
      flows_overland_eu <- overland_eu.get_flows() %>%
        mutate(
          entry_mode = "N/A",
          process = "overland_eu"
        )
      db.upload_flows_to_postgres(flows_overland_eu, production = T)
    }),
    list(name = "Updating China flows", code = function() {
      flows_china <- china.get_flows() %>%
        mutate(
          value_mwh = NA_real_,
          entry_mode = "N/A",
          process = "china"
        )
      db.upload_flows_to_postgres(flows_china, production = T)
    }),
    list(name = "Updating India flows", code = function() {
      flows_turkey <- turkey.get_flows() %>%
        mutate(
          value_mwh = NA_real_,
          entry_mode = "N/A",
          process = "turkey"
        )
      db.upload_flows_to_postgres(flows_turkey, production = T)
    })
  )

  log_info("Running stages")

  failed_stages <- c()
  for (stage in stages) {
    log_level(STAGE, stage$name)
    tryCatch(
      {
        stage$code()
      },
      error = function(e) {
        failed_stages <<- c(failed_stages, stage$name)
        log_level(STAGE, paste("Failed:", stage$name, "with error:", e$message))
      }
    )
  }

  if (length(failed_stages) > 0) {
    stop(paste("The following stages have failed:", paste(failed_stages, collapse = ", ")))
  }
}
