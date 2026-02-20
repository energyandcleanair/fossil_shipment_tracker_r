price_models.build_models <- function(diagnostic_folder = "diagnostics") {
  price_models_comtrade.build(diagnostic_folder = diagnostic_folder)
  price_models_eurostat.build(diagnostic_folder = diagnostic_folder)
}

price_models.get_predicted <- function(add_urals_espo = T, prices_daily_30day = NULL) {
  log_info("Get predicted prices for comtrade")
  comtrade <- price_models_comtrade.get_predicted(prices_daily_30day = prices_daily_30day)
  log_info("Get predicted prices for eurostat")
  eurostat <- price_models_eurostat.get_predicted()

  predicted <- bind_rows(comtrade, eurostat)

  if (add_urals_espo) {
    log_info("Adding predicted prices for Urals and Espo")
    # Add Ural and Espo price
    # Not really predicted...
    p_espo <- get_espo() %>%
      mutate(commodity = "crude_oil_espo") %>%
      fill_gaps_and_future()

    p_urals <- get_urals() %>%
      mutate(commodity = "crude_oil_urals") %>%
      fill_gaps_and_future()

    predicted <- bind_rows(
      predicted,
      p_espo,
      p_urals
    )
  }

  return(predicted)
}
