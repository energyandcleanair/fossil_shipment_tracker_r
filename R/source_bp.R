bp.get_flows <- function(use_cache=T){

  f <- "data/bp-stats-review-2021-all-data.xlsx"
  sheet <- "Gas - Trade movts - pipeline"


#
#   # natural_gas
#   readxl::read_xlsx(f,
#                     sheet="Gas - Trade movts - pipeline",
#                     range="A3:W41") %>%
#     tidyr::pivot_longer(cols = -To,
#                         names_to="partner") %>%
#     rename(country=To) %>%
#     mutate(commodity=commodity,
#            unit="m3",
#            value=value*1e9) %>%
#     filter(!country %in% c("North America", "S. & Cent. America", "Europe",
#                            "CIS","Middle East","Middle East", "Asia Pacific")) %>%
#     mutate(commodity="natural_gas"),
#
#
#   #lng
#   readxl::read_xlsx(f,
#                     sheet="Gas - Trade movts LNG",
#                     range="A4:W38") %>%
#     tidyr::pivot_longer(cols = -To,
#                         names_to="partner") %>%
#     rename(country=To) %>%
#     mutate(commodity=commodity,
#            unit="m3",
#            value=value*1e9) %>%
#     filter(!country %in% c("North America", "S. & Cent. America", "Europe",
#                            " Middle East & Africa"," Asia Pacific ")) %>%
#     mutate(commodity="lng")
#
#
#
#
#
#
#     filter(!is.na(value))
#
#
#
#
#   lapply(names(commodity_sheet), function(commodity){
#     sheet <- commodity_sheet[[commodity]]
#     readxl::read_xlsx(f,
#                       sheet=sheet,
#                       skip=as.integer(sheet_skip[[sheet]])) %>%
#       tidyr::pivot_longer(cols = -To,
#                           names_to="partner") %>%
#       rename(country=To) %>%
#       mutate(commodity=commodity,
#              unit=sheet_unit[[sheet]]) %>%
#       filter(!country %in% c("North America", "S. & Cent. America", "Europe",
#                              "CIS","Middle East","Middle East", "Asia Pacific"),
#              !grepl("Total",country)) %>%
#       filter(!is.na(value))
#   })
#
#
#
#   commodity_codes <- list(
#     "G3000"="natural_gas",
#     "G3200"="lng",
#     "O4100_TOT"="crude_oil",
#     "2210"="lignite",
#     "2111"="coal")
#
#   units <- list(
#     "MIO_M3"="m3",
#     "TJ_GCV"="TJ",
#     "THS_T"="tonne"
#   )
#
#   unit_factors <- list(
#     "MIO_M3"=1e6,
#     "TJ_GCV"=1,
#     "THS_T"=1
#   )
#
#   f <- "cache/eurostat.RDS"
#   if(use_cache && file.exists(f)){ return(readRDS(f)) }
#
#   gas_import <- eurostat::get_eurostat("nrg_ti_gasm")
#   gas_export <- eurostat::get_eurostat("nrg_te_gasm") %>% mutate(values=values*-1)
#
#   oil_import <- eurostat::get_eurostat("nrg_ti_oilm")
#   oil_export <- eurostat::get_eurostat("nrg_te_oilm") %>% mutate(values=values*-1)
#
#   coal_import <- eurostat::get_eurostat("nrg_122m") %>% rename(siec=product)
#   coal_export <- eurostat::get_eurostat("nrg_132m") %>% rename(siec=product) %>% mutate(values=values*-1)
#
#   data <- bind_rows(gas_import, gas_export,
#                     oil_import, oil_export,
#                     coal_import, coal_export) %>%
#     rename(value=values,
#            date=time) %>%
#     filter(siec %in% names(commodity_codes)) %>%
#     mutate(value=recode(unit, !!!unit_factors)*value,
#            unit=recode(unit, !!!units),
#            commodity=recode(siec, !!!commodity_codes)) %>%
#     filter(value>0) %>%
#     filter(!grepl('EU|EA19|EA', geo), partner != 'TOTAL') %>%
#     # filter(partner %in% c("RU","BY","UA")) %>%
#     mutate(country=countrycode::countrycode(geo, "iso2c", "country.name",
#                                             custom_match = c("UK"="United Kingdom", "EL"="Greece", "XK"="XK"))) %>%
#     mutate(partner=countrycode::countrycode(partner, "iso2c", "country.name")) %>%
#     # mutate(value=values * 1e6 / MJ_per_kWh / 1000,
#     #        unit="MWh/month") %>%
#     group_by(country, unit, partner, date, commodity) %>%
#     summarise(value=sum(value)) %>%
#     mutate(source='Eurostat') %>%
#     ungroup()
#
#   saveRDS(data, f)
#   return(data)
}
