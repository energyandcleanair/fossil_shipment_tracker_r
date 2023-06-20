build_price_models <- function(production=F, refresh_trade=T, diagnostic_folder='diagnostics'){
  price_models_comtrade.build(production=production, refresh_comtrade = refresh_trade, diagnostic_folder = diagnostic_folder)
  price_models_eurostat.build(production=production, diagnostic_folder = diagnostic_folder)
}
