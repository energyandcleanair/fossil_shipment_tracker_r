# Using devtools, reinstall the russiacounter package from the local directory
devtools::load_all(".")

price_models_eurostat.build(
    production = T
)

price_models_comtrade.build(
    production = T
)
