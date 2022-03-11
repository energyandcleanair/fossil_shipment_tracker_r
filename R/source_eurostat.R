eurostat.get_flows <- function(use_cache=T){

  commodity_codes <- list(
    "G3000"="natural_gas",
    "G3200"="lng",
    "O4100_TOT"="crude_oil",
    "2210"="lignite",
    "2111"="coal")

  units <- list(
    "MIO_M3"="m3",
    "TJ_GCV"="TJ",
    "THS_T"="tonne"
  )

  unit_factors <- list(
    "MIO_M3"=1e6,
    "TJ_GCV"=1,
    "THS_T"=1
  )

  f <- "cache/eurostat.RDS"
  if(use_cache && file.exists(f)){ return(readRDS(f)) }

  gas_import <- eurostat::get_eurostat("nrg_ti_gasm")
  gas_export <- eurostat::get_eurostat("nrg_te_gasm") %>% mutate(values=values*-1)

  oil_import <- eurostat::get_eurostat("nrg_ti_oilm")
  oil_export <- eurostat::get_eurostat("nrg_te_oilm") %>% mutate(values=values*-1)

  coal_import <- eurostat::get_eurostat("nrg_122m") %>% rename(siec=product)
  coal_export <- eurostat::get_eurostat("nrg_132m") %>% rename(siec=product) %>% mutate(values=values*-1)

  data <- bind_rows(gas_import, gas_export,
                    oil_import, oil_export,
                    coal_import, coal_export) %>%
    rename(value=values,
           date=time) %>%
    filter(siec %in% names(commodity_codes)) %>%
    mutate(value=recode(unit, !!!unit_factors)*value,
           unit=recode(unit, !!!units),
           commodity=recode(siec, !!!commodity_codes)) %>%
    filter(value>0) %>%
    filter(!grepl('EU|EA19|EA', geo), partner != 'TOTAL') %>%
    # filter(partner %in% c("RU","BY","UA")) %>%
    mutate(country=countrycode::countrycode(geo, "iso2c", "country.name",
                                            custom_match = c("UK"="United Kingdom", "EL"="Greece", "XK"="XK"))) %>%
    mutate(partner=countrycode::countrycode(partner, "iso2c", "country.name")) %>%
    # mutate(value=values * 1e6 / MJ_per_kWh / 1000,
    #        unit="MWh/month") %>%
    group_by(country, unit, partner, date, commodity) %>%
    summarise(value=sum(value)) %>%
    mutate(source='Eurostat') %>%
    ungroup()

  saveRDS(data, f)
  return(data)
}
