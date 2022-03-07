get_eurostat <- function(years=seq(2017,2020), gcv_kWh_per_m3=11.259){

  usd_per_eur <- getSymbols("EUR=X", from=paste0(min(years),"-01-01"), to= paste0(max(years),"-12-31"), auto.assign = F)
  usd_per_eur <- tibble(date=index(usd_per_eur), value=as.numeric(usd_per_eur$`EUR=X.Adjusted`)) %>%
    group_by(date=lubridate::floor_date(date, 'month')) %>%
    summarise(usd_per_eur=mean(value, na.rm=T))

  require(eurostat)
  search_eurostat('solid.*fuels.*partner', fixed=F)
  exim_codes <- search_eurostat('(Imports|Exports).*partner', fixed=F) %>% data.frame %>% filter(data.end=='2020')
  exim <- exim_codes$code %>% lapply(get_eurostat, select_time='M')
  names(exim) <- exim_codes$code

  exim %>% bind_rows(.id='code') %>% filter(year(time)%in%years) %>% filter(values != 0) -> d
  d %<>% left_join(exim_codes %>% select(code, title))
  d %>% filter(values != 0) %>%
    mutate(values = values * ifelse(grepl('Imports', title), 1, -1),
           product = gsub('.* of | by .*', '', title)) %>%
    group_by(geo, partner, unit, product, time) %>%
    summarise(value=sum(values, na.rm=T)) ->
    net_imports

  net_imports %>%
    filter(!grepl('EU|EA19', geo), partner != 'TOTAL', !grepl('biofuels|electricity', product)) %>%
    filter(partner=="RU") %>%
    ungroup() %>%
    mutate(country=countrycode::countrycode(geo, "iso2c", "country.name", custom_match = c("UK"="United Kingdom", "EL"="EL", "XK"="XK"))) %>%
    mutate(value=ifelse(unit=="MIO_M3", value * gcv_kWh_per_m3 / 1000 * 1e6, value),
           unit=ifelse(unit=="MIO_M3", "MWh", unit))


  # net_imports %>% filter(!grepl('EU|EA19', geo), !grepl('elect', product), partner != 'TOTAL', , value>0) %>%
  #   mutate(import_from=ifelse(partner %in% top_partners$partner, partner, 'others')) %>%
  #   group_by(geo, import_from, product, time) %>%
  #   summarise(across(value, sum, na.rm=T)) %>% ungroup %>%
  #   mutate(across(c(geo, import_from), countrycode, 'iso2c', 'country.name.en',
  #                 custom_match = c(NSP='not specified', OTHERS='others', EL='Greece', 'XK'='Kosovo', UK='UK'))) %>%
  #   ggplot(aes(geo, value, fill=import_from)) + geom_col() + facet_wrap(~product, scales = 'free_x') + coord_flip() +
  #   scale_x_discrete(limits=rev)
}
