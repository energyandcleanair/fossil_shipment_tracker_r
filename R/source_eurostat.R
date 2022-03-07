eurostat.get_flows <- function(use_cache=T){

  # usd_per_eur <- getSymbols("EUR=X", from=paste0(min(years),"-01-01"), to= paste0(max(years),"-12-31"), auto.assign = F)
  # usd_per_eur <- tibble(date=index(usd_per_eur), value=as.numeric(usd_per_eur$`EUR=X.Adjusted`)) %>%
  #   group_by(date=lubridate::floor_date(date, 'month')) %>%
  #   summarise(usd_per_eur=mean(value, na.rm=T))

  # search_eurostat('solid.*fuels.*partner', fixed=F)
  # exim_codes <- search_eurostat('(Imports|Exports).*partner', fixed=F) %>% data.frame %>% filter(data.end=='2020')
  # exim <- exim_codes$code %>% lapply(get_eurostat, select_time='M')
  # names(exim) <- exim_codes$code
#
#   exim %>% bind_rows(.id='code') %>% filter(year(time)%in%years) %>% filter(values != 0) -> d
#   d %<>% left_join(exim_codes %>% select(code, title))
#   d %>% filter(values != 0) %>%
#     mutate(values = values * ifelse(grepl('Imports', title), 1, -1),
#            product = gsub('.* of | by .*', '', title)) %>%
#     group_by(geo, partner, unit, product, time) %>%
#     summarise(value=sum(values, na.rm=T)) ->
#     net_imports
#
#   net_imports %>%
#     filter(!grepl('EU|EA19', geo), partner != 'TOTAL', !grepl('biofuels|electricity', product)) %>%
#     filter(partner=="RU") %>%
#     ungroup() %>%
#     mutate(country=countrycode::countrycode(geo, "iso2c", "country.name", custom_match = c("UK"="United Kingdom", "EL"="EL", "XK"="XK"))) %>%
#     mutate(value=ifelse(unit=="MIO_M3", value * gcv_kWh_per_m3 / 1000 * 1e6, value),
#            unit=ifelse(unit=="MIO_M3", "MWh", unit))

  # net_imports %>% filter(!grepl('EU|EA19', geo), !grepl('elect', product), partner != 'TOTAL', , value>0) %>%
  #   mutate(import_from=ifelse(partner %in% top_partners$partner, partner, 'others')) %>%
  #   group_by(geo, import_from, product, time) %>%
  #   summarise(across(value, sum, na.rm=T)) %>% ungroup %>%
  #   mutate(across(c(geo, import_from), countrycode, 'iso2c', 'country.name.en',
  #                 custom_match = c(NSP='not specified', OTHERS='others', EL='Greece', 'XK'='Kosovo', UK='UK'))) %>%
  #   ggplot(aes(geo, value, fill=import_from)) + geom_col() + facet_wrap(~product, scales = 'free_x') + coord_flip() +
  #   scale_x_discrete(limits=rev)
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
    filter(partner %in% c("RU","BY","UA")) %>%
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
