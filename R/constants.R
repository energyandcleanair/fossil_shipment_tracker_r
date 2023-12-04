MJ_per_kWh <- 3.6
kg_per_m3 <- 0.717
gcv_kWh_per_m3 <- 11.259
gcv_MWh_per_m3 <- gcv_kWh_per_m3 / 1e3
gcv_MJ_per_m3 <- gcv_kWh_per_m3 * 3.6 #40
barrel_per_tonne <- 1 / 0.138

eu_iso2s <- setdiff(countrycode::codelist$iso2c[!is.na(countrycode::codelist$eu28)], 'GB')
g7_iso2s <- c('CA','FR','DE','IT','JP','GB','US','UK')
eu_g7_iso2s <- c(eu_iso2s, g7_iso2s)

date_from_counter <- "2022-02-24"
codelist <- countrycode::codelist

sources <- c(
  "ENTSO-G"="entsog",
  "EUROSTAT"="eurostat"
)

commodities <- c(
  "Fossil Gas"="gas_all",
  "Fossil Gas (pipeline)"="natural_gas",
  "LNG"="lng",
  "Coal gas"="coal_gas",
  "Crude Oil"="crude_oil",
  "Lignite"="lignite",
  "Hard Coal"="coal",
  "Oil Products"="oil_products",
  "Oil (Others)"="oil_others",
  "Crude Oil & Oil products"="oil",
  "Coke"="coke",
  "Coal & Coke"="coal_coke",
  "Tar"="tar"
)


hs_commodities <- c(
  "2711"="gas_all",
  "271121"="natural_gas",
  "271111"="lng",
  "2701"="coal",
  "2704"="coke",
  "2710"="oil_products",
  "2709"="oil",
  "2705"="coal_gas",
  "2706"="tar"
)



# Iso2s of importing countries we're interested in
eugb_iso2s <- c("AT", "BE", "BG", "HR", "CY", "CZ", "DK", "EE", "FI", "FR", "DE", "GR", "HU",
                "IE", "IT", "LV", "LT", "LU", "MT", "NL", "PL", "PT", "RO", "SK", "SI", "ES",
                "SW", "GB", "UK")
