MJ_per_kWh <- 3.6
kg_per_m3 <- 0.717
gcv_kWh_per_m3 <- 11.259
gcv_MWh_per_m3 <- gcv_kWh_per_m3 / 1e3
gcv_MJ_per_m3 <- gcv_kWh_per_m3 * 3.6 #40


sources <- c(
  "ENTSO-G"="entsog",
  "EUROSTAT"="eurostat"
)

commodities <- c(
  "Fossil Gas"="natural_gas",
  "Liquefied Natural Gas"="lng",
  "Crude Oil"="crude_oil",
  "Lignite"="lignite",
  "Hard Coal"="coal",
  "Oil Products"="oil_products"
)

# Iso2s of importing countries we're interested in
iso2s <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU",
           "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES", "SW", "GB"
)
