comtrade_eurostat_russia.get_flows <- function(use_cache=F){

  f_cache <- "cache/comtrade_eurostat_russia.RDS"
  if(use_cache && file.exists(f_cache)){ return(readRDS(f_cache)) }


  oil_codes <- c("2709","2710")
  gas_codes <- c("2711","271121","271111")
  coal_codes <- c("2701","2704","2705","2706")

  exports_as_neg <- function(df) {
    df %>% mutate(netweight_kg=netweight_kg*ifelse(trade_flow=="Export",-1, 1),
                  trade_value_usd=trade_value_usd*ifelse(trade_flow=="Export",-1,1))
  }

  add_prod <- function(df) {
    df %>% mutate(product = case_when(grepl('Coal|Coke|from coal', commodity)~'coal&coke',
                                      grepl('crude$', commodity)~'crude oil',
                                      grepl('not crude|other than crude', commodity)~'oil products',
                                      grepl('liquefied.*gas', commodity)~'LNG',
                                      grepl('gaseous.*gas', commodity)~'pipeline gas',
                                      grepl('gases.*hydrocarbons$', commodity)~'fossil gas total'))
  }

  sum_q = function(df, fun=sum, na.rm=T) df %>% summarise(across(c(trade_value_usd, netweight_kg), fun, na.rm=na.rm))


  comtradr::ct_search(partners = comtradr::ct_country_lookup("Russia"),
            reporters = "all",
            trade_direction = "all",
            commod_codes=c(coal_codes, oil_codes, gas_codes),
            freq="annual",
            start_date = 2019,
            end_date = 2021) -> imports_from_russia

  comtradr::ct_search(partners = "World",
            reporters = "all",
            trade_direction = "all",
            commod_codes=c(coal_codes, oil_codes, gas_codes),
            freq="annual",
            start_date = 2019,
            end_date = 2021) -> total_imports

  total_imports %<>% add_prod()
  imports_from_russia %<>% add_prod()

  total_imports %>% exports_as_neg() %>% group_by(reporter, reporter_iso, partner, product, year) %>%
    sum_q() -> total_imports_net

  imports_from_russia %>% exports_as_neg() %>% group_by(reporter, reporter_iso, partner, product, year) %>%
    sum_q() -> imports_ru_net


  eurostat::search_eurostat('(Imports|Exports).*partner', fixed=F) %>% data.frame %>% filter(data.end=='2020') -> exim_codes

  exim_codes$code %>% lapply(eurostat::get_eurostat) -> exim
  names(exim) <- exim_codes$code
  exim %>% bind_rows(.id='code') %>% filter(year(time)==2019) %>% filter(values != 0) -> eurostat_data
  eurostat_data %<>% left_join(exim_codes %>% select(code, title))

  eurostat_data %>%
    filter(!grepl('EU|EA19', geo), code=='nrg_ti_gas', unit=='MIO_M3',
           partner %in% c('RU', 'TOTAL'), grepl('Imports', title),
           year(time)==2019) %>%
    group_by(geo, partner) %>% summarise(across(values, sum)) %>% group_by(geo) %>%
    summarise(share_of_ru = values[partner=='RU']/values[partner=='TOTAL']) %>%
    bind_rows(tibble(geo='AT', share_of_ru=.8)) ->
    gas_share_from_ru
  #reference for Austria https://www.thelocal.at/20220303/how-reliant-is-austria-on-russia-for-energy/

  total_imports_net %>% filter(product == 'fossil gas total') %>%
    inner_join(gas_share_from_ru %>%
                 mutate(reporter_iso=countrycode(geo, 'iso2c', 'iso3c',
                                                 custom_match=c(UK='GBR', EL='GRC')))) %>%
    mutate(across(c(trade_value_usd, netweight_kg), magrittr::multiply_by, share_of_ru),
           partner='Russian Federation') -> ru_adjusted

  imports_ru_net %>% anti_join(ru_adjusted %>% select(reporter, partner, product, year)) %>%
    bind_rows(ru_adjusted) -> ru_adjusted

  ru_adjusted %<>% group_by(reporter) %>%
    group_modify(function(df, ...) {
      totgas <- sum(df$trade_value_usd[df$product=='fossil gas total'])
      pipe_lng <- sum(df$trade_value_usd[df$product %in% c('pipeline gas', 'LNG')])
      to_excl = ifelse(pipe_lng > totgas, 'gas total', 'pipeline|LNG')
      df %>% filter(!grepl(to_excl, product))
    })


  ru_adjusted %<>% mutate(EU = reporter_iso %in% codelist$iso3c[!is.na(codelist$eu28)] & reporter_iso != 'GBR')
  ru_adjusted %<>% filter(EU) %>%
    group_by(partner, product, year) %>% sum_q %>%
    mutate(reporter='EU', reporter_iso='EUU') %>%
    bind_rows(ru_adjusted %>% filter(reporter_iso!='EUU'))

  ru_adjusted %>%
    group_by(reporter, year) %>% sum_q() %>%
    group_by(reporter) %>% sum_q(mean) %>%
    arrange(desc(trade_value_usd)) -> top_importers_from_russia



  flows <- ru_adjusted %>%
    ungroup() %>%
    mutate(partner=ifelse(grepl("Russia",partner), "Russia", partner),
           date=paste0(year,"-01-01"),
           value_tonne=netweight_kg/1000,
           commodity=recode(product,
                            `coal&coke`="coal_coke",
                            `crude oil`="crude_oil",
                            `fossil gas total`="gas_all",
                            `LNG`='lng',
                            `oil products`="oil_products",
                            `pipeline gas`="natural_gas"
                            )
           ) %>%
    select(country=reporter,
           partner,
           date,
           commodity,
           value_usd=trade_value_usd,
           value_tonne
           ) %>%
    tidyr::pivot_longer(cols=c(value_usd, value_tonne),
                        names_to="unit",
                        names_prefix="value_") %>%
    mutate(source="comtrade_eurostat_russia")

  saveRDS(flows, f_cache)
  return(flows)

  # ru_adjusted %>%
  #   group_by(reporter, year, product) %>% sum_q() %>%
  #   group_by(reporter, product) %>% sum_q(mean) %>%
  #   ggplot(aes(reporter, trade_value_usd/1e9/1.1, fill=product)) + geom_col() + coord_flip() +
  #   rcrea::scale_fill_crea_d('dramatic', col.index = c(1:7)) +
  #   scale_x_discrete(limits=top_importers_from_russia$reporter[1:20] %>% rev) +
  #   theme_crea() +
  #   scale_y_continuous(expand=expansion(mult=c(0,.05))) +
  #   labs(title='Largest importers of fossil fuels from Russia', x='', y='bln EUR/year',
  #        subtitle='2019–2021 average')
  #
  # ggsave('Largest importers of fossil fuels from Russia.png')
  #
  #
  #
  #
  # ru_adjusted %>% filter(EU) %>%
  #   mutate(product = case_when(grepl('oil', product)~'oil',
  #                              grepl('coal', product)~'coal',
  #                              T~'gas')) %>%
  #   group_by(reporter, year, product) %>% sum_q() %>%
  #   group_by(reporter, product) %>% summarise(value = sum(trade_value_usd, na.rm=T) / 1.1 / 1e9) %>%
  #   spread(product, value) %>%
  #   mutate(across(is.numeric, function(x) {x[x<0] <- NA; return(x)})) -> ebc_table
  #
  # ebc_table %>% ungroup %>% select(is.numeric) %>% rowSums(na.rm=T) -> ebc_table$total
  #
  # ebc_table %>%
  #   mutate(unit="bln EUR") %>%
  #   arrange(desc(total)) %>% write_csv('EU fossil fuel imports from RU, by country, bln EUR.csv')


}



