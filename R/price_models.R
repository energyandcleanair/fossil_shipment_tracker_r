price_models.build_models <- function(production=F, refresh_trade=T, diagnostic_folder='diagnostics'){
  price_models_comtrade.build(production=production, refresh_comtrade = refresh_trade, diagnostic_folder = diagnostic_folder)
  price_models_eurostat.build(production=production, diagnostic_folder = diagnostic_folder)
}

price_models.get_predicted <- function(production=F, add_urals_espo=T, prices_daily_30day=NULL){

  comtrade <- price_models_comtrade.get_predicted(production=production, prices_daily_30day=prices_daily_30day)
  eurostat <- price_models_eurostat.get_predicted(production=production)

  predicted <- bind_rows(comtrade, eurostat)

  if(add_urals_espo){
    # Add Ural and Espo price
    # Not really predicted...
    p_espo <- get_espo() %>%
      mutate(commodity='crude_oil_espo') %>%
      filter(!is.na(eur_per_tonne)) %>%
      fill_gaps_and_future()

    p_urals <- get_urals() %>%
      mutate(commodity='crude_oil_urals') %>%
      filter(!is.na(eur_per_tonne)) %>%
      fill_gaps_and_future()

    predicted <- bind_rows(
      predicted,
      p_espo,
      p_urals
    )
  }

  return(predicted)
}

