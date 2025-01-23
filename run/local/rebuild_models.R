# Using devtools, reinstall the russiacounter package from the local directory
devtools::load_all(".")

brent_to_eur <- price_models_eurostat.build(
    production = T
)
