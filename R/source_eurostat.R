eurostat.get_flows <- function(use_cache=T){

  commodity_codes <- list(
    "G3000"="gas_all",
    "G3200"="lng",
    "O4100_TOT"="crude_oil",
    "O4600"="oil_products",
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
    "THS_T"=1000
  )

  f <- "cache/eurostat.RDS"
  if(use_cache && file.exists(f)){ return(readRDS(f)) }

  gas_import <- eurostat::get_eurostat("nrg_ti_gasm", update_cache=T)
  gas_export <- eurostat::get_eurostat("nrg_te_gasm", update_cache=T) %>% mutate(values=values*-1)

  oil_import <- eurostat::get_eurostat("nrg_ti_oilm", update_cache=T)
  oil_export <- eurostat::get_eurostat("nrg_te_oilm", update_cache=T) %>% mutate(values=values*-1)

  coal_import <- eurostat::get_eurostat("nrg_122m", update_cache=T) %>% rename(siec=product)
  coal_export <- eurostat::get_eurostat("nrg_132m", update_cache=T) %>% rename(siec=product) %>% mutate(values=values*-1)

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
  saveRDS(data, "inst/extdata/eurostat.RDS")
  return(data)
}


eurostat.get_gas_flows <- function(use_cache=T){

  gas_consumption <- eurostat::get_eurostat("nrg_cb_gasm", update_cache=!use_cache)
  gas_consumption %>%
    filter(unit=='MIO_M3',
           siec=="G3000") %>%
    mutate(type=recode(nrg_bal,
      IC_OBS='consumption',
      IPRD='production',
      IMP='imports',
      EXP='minus_exports',
      STK_CHG_MG='storage_drawdown',
      .default=NA_character_,
      )) %>%
    filter(!is.na(type)) %>%
    mutate(values=ifelse(type %in% c('minus_exports', 'storage_drawdown'), -values, values)) %>%
    select(iso2=geo, date=time, value=values, type) %>%
    mutate(value_m3=value*1e6,
           unit='m3',
           iso2=recode(iso2, 'UK'='GB'),
           country=countrycode::countrycode(iso2, 'iso2c', 'country.name')) %>%
    filter(!is.na(country)) %>%
    select(-c(value))
}
