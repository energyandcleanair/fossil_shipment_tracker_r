comtrade_eurostat.get_flows <- function(use_cache=F){

  f_cache <- "cache/comtrade_eurostat.RDS"
  if(use_cache && file.exists(f_cache)){ return(readRDS(f_cache)) }

  oil_codes <- c("2709","2710")
  gas_codes <- c("2711","271121","271111")
  coal_codes <- c("2701","2704","2705","2706")

  exports_as_neg <- function(df) {
    df %>% mutate(netweight_kg=netweight_kg*ifelse(trade_flow=="Export",-1, 1),
                  trade_value_usd=trade_value_usd*ifelse(trade_flow=="Export",-1,1))
  }

  set_date <- function(df){
    df %>%
      mutate(date=as.Date(strptime(paste0(period,"01"), "%Y%m%d")))
  }

  add_commodity <- function(df) {
    df %>%
      filter(commodity_code %in% names(hs_commodities)) %>%
      mutate(commodity=recode(commodity_code, !!!hs_commodities))
  }

  set_reporter_iso <- function(df) {
    df %>%
      mutate(reporter_iso = countrycode(reporter, "country.name", "iso2c", custom_match = c(`EU-28`="EUU")))
  }


  sum_q = function(df, fun=sum, na.rm=T) df %>% summarise(across(c(trade_value_usd, netweight_kg), fun, na.rm=na.rm))


  # Collect comtrade data ---------------------------------------------------
  annual_countries <- c("China","Austria","Denmark","Ireland","Malta")
  f_imports_russia <- "cache/comtrade_monthly_from_russia.RDS"
  if(file.exists(f_imports_russia)){
    imports_from_russia <- readRDS(f_imports_russia)
  }else{
    # Most counties except China seem to have monthly data
    imports_from_russia <- utils.collect_comtrade(partners=comtradr::ct_country_lookup("Russia"),
                                                    reporters="all",
                                                    years=seq(2016, 2021),
                                                  frequency="monthly",
                                                  codes=c(oil_codes, gas_codes, coal_codes)) %>%
      filter(!reporter %in% annual_countries)

    # Some country have no monthly data. We take years and split equally
    import_from_russia_china <- utils.collect_comtrade(partners=comtradr::ct_country_lookup("Russia"),
                                                       reporters=annual_countries,
                                                       years=seq(2016, 2021),
                                                       frequency="annual",
                                                       codes=c(oil_codes, gas_codes, coal_codes)) %>%
      select(-c(period)) %>%
      left_join(tibble(period=seq(as.Date("2016-01-01"),as.Date("2021-12-31"),by="month")) %>%
                  mutate(year=lubridate::year(period),
                         period=as.integer(strftime(period, "%Y%m")))
                ) %>%
      mutate(netweight_kg=netweight_kg/12,
             trade_value_usd=trade_value_usd/12)

    imports_from_russia %<>% bind_rows(import_from_russia_china)
    saveRDS(imports_from_russia, f_imports_russia)
  }

  f_imports_world <- "cache/comtrade_monthly_from_world.RDS"
  if(file.exists(f_imports_world)){
    imports_world <- readRDS(f_imports_world)
  }else{
    imports_world <- utils.collect_comtrade(partners="World",
                                            reporters="all",
                                            years=seq(2016, 2021),
                                            frequency="monthly",
                                            codes=c(oil_codes, gas_codes, coal_codes)) %>%
      filter(!reporter %in% annual_countries)

    # Some country have no monthly data. We take years and split equally
    imports_world_china <- utils.collect_comtrade(partners="World",
                                                       reporters=annual_countries,
                                                       years=seq(2016, 2021),
                                                       frequency="annual",
                                                       codes=c(oil_codes, gas_codes, coal_codes)) %>%
      select(-c(period)) %>%
      left_join(tibble(period=seq(as.Date("2016-01-01"),as.Date("2021-12-31"),by="month")) %>%
                  mutate(year=lubridate::year(period),
                         period=as.integer(strftime(period, "%Y%m")))
      ) %>%
      mutate(netweight_kg=netweight_kg/12,
             trade_value_usd=trade_value_usd/12)

    imports_world %<>% bind_rows(imports_world_china)
    saveRDS(imports_world, f_imports_world)
  }

  imports_world %<>% add_commodity() %>% set_date()  %>% set_reporter_iso()
  imports_from_russia %<>% add_commodity() %>% set_date() %>% set_reporter_iso()

  imports_world %>% exports_as_neg() %>% group_by(reporter, reporter_iso, partner, commodity, date) %>%
    sum_q() -> imports_world_net

  imports_from_russia %>% exports_as_neg() %>% group_by(reporter, reporter_iso, partner, commodity, date) %>%
    sum_q() -> imports_ru_net


  # Use Eurostat to fix gas share from Russia -----------------------------------
  f_eurostat <- "cache/eurostatdata.RDS"
  if(!file.exists(f_eurostat)){
    eurostat_codes <- c("nrg_ti_gasm", "nrg_te_gasm")
    exim <- eurostat_codes %>% lapply(eurostat::get_eurostat)
    names(exim) <- eurostat_codes

    eurostat_data <- exim %>% bind_rows(.id='code') %>%
      filter(lubridate::year(time) %in% seq(2016,2021)) %>% filter(values != 0)
    saveRDS(eurostat_data, f_eurostat)
  }else{
    eurostat_data <- readRDS(f_eurostat)
  }


  gas_share_from_ru <- eurostat_data %>%
    filter(!grepl('EU|EA19', geo), code=='nrg_ti_gasm', unit=='MIO_M3',
           partner %in% c('RU', 'TOTAL')) %>%
    select(siec, partner, unit, geo, time, values) %>%
    tidyr::spread(siec, values) %>%
    mutate(GPIPELINE=G3000-G3200) %>%
    tidyr::gather("siec","values",c("G3000","G3200","GPIPELINE")) %>%
    group_by(geo, siec, partner, time) %>%
    summarise(across(values, sum)) %>%
    group_by(geo, siec, time) %>%
    summarise(share_of_ru = values[partner=='RU']/values[partner=='TOTAL']) %>%
    mutate(commodity=recode(siec, G3000="gas_all", G3200="lng", GPIPELINE="natural_gas")) %>%
    bind_rows(
      tibble(geo=c('AT','HR'), share_of_ru=c(.8,.7)) %>% tidyr::crossing(commodity=c("gas_all","natural_gas"))
      ) %>%
    filter(!is.na(commodity)) %>%
    ungroup() %>%
    rename(reporter_iso=geo) %>%
    mutate(reporter_iso=recode(reporter_iso, UK="GB", EL="GR")) %>%
    select(-c(siec)) %>%
    rename(date=time)
    #reference for Austria https://www.thelocal.at/20220303/how-reliant-is-austria-on-russia-for-energy/

  ru_adjusted <- imports_world_net %>%
    ungroup() %>%
    inner_join(gas_share_from_ru) %>%
    mutate(across(c(trade_value_usd, netweight_kg), magrittr::multiply_by, share_of_ru),
           partner='Russian Federation')

  ru_adjusted <- imports_ru_net %>%
    anti_join(ru_adjusted %>% select(reporter, partner, commodity, date)) %>%
    bind_rows(ru_adjusted)

  # Gas = pipeline + lng
  ru_adjusted %>%
    filter(grepl("gas|lng",commodity)) %>%
    group_by(reporter, year=lubridate::date(date), partner, commodity ) %>%
    summarise(value=sum(trade_value_usd, na.rm=T)) %>%
    spread(commodity, value) %>%
    # filter(reporter %in% c(annual_countries, "Austria")) %>%
    filter(reporter=="Netherlands") %>%
    View()

  # Looking at each country, we deduct a rule to split
  countries_keep_pipelined_only <- c("Armenia", "Bosnia Herzegovina", "Belarus",
                                     "Switzerland", "Georgia", "Kyrgyzstan",
                                     "Kazakhstan", "Uzbekistan")

  countries_keep_lng_only <-c("Spain","India","Japan","Rep. of Korea",
                              "Norway","Pakistan","Portugal", "United Kingdom")

  countries_all_is_pipelined <- c("Azerbaijan", "Bulgaria", "Czech Rep.","Hungary","Lithuania",
                                  "Rep. of Moldova","Montenegro","TFYR of Macedonia","Mongolia","Poland","Romania","Serbia","Slovenia","Slovakia", "Ukraine", "Luxembourg","Germany","Austria")
  countries_all_is_lng<- c("Côte d'Ivoire", "Cyprus", "Egypt", "Israel", "Morocco", "United States of America", "South Africa", "Ireland")
  countries_pipelined_equals_total_minus_lng <- c("Belgium", "Finland", "Croatia", "Sweden", "China", "Netherlands")
  countries_split_is_correct <- c("Estonia", "France", "Latvia", "China", "Italy", "Greece")
  countries_impose_pipeline_share <- c("Turkey"=0.77) #https://www.mees.com/2022/3/4/economics-finance/turkey-2021-gas-imports-ukraine-war-puts-russia-reliance-in-focus/dd8176c0-9bcd-11ec-96b1-3f07b501dca9#:~:text=*Turkey%20imported%20a%20record%2060.1,in%20the%20form%20of%20LNG.

  # Remaining:EU-28
  # To solve:
  # -Greece gas probably transiting in Turkey.
  # -UK has a very significant non-lng part. What is it??
   ru_adjusted <- ru_adjusted %>% group_by(reporter) %>%
     group_modify(function(df, reporter) {
       r <- unique(reporter$reporter)

       if(r %in% countries_keep_pipelined_only){
         return(df %>% filter(!commodity %in% c("gas_all","lng")))
       }

       if(r %in% countries_keep_lng_only){
         return(df %>% filter(!commodity %in% c("gas_all","natural_gas")))
       }

       if(r %in% countries_all_is_pipelined){
         return(df %>%
                  filter(!commodity %in% c("lng","natural_gas")) %>%
                  mutate(commodity=recode(commodity, gas_all="natural_gas")))
       }

       if(r %in% countries_all_is_lng){
         return(df %>%
                  filter(!commodity %in% c("lng","natural_gas")) %>%
                  mutate(commodity=recode(commodity, gas_all="lng")))
       }

       if(r %in% countries_pipelined_equals_total_minus_lng){
         return(bind_rows(
           df %>% filter(commodity != "natural_gas"),
                   bind_rows(df %>% filter(commodity=="gas_all"),
                             df %>% filter(commodity=="lng") %>% mutate(trade_value_usd=-trade_value_usd,
                                                                        netweight_kg=-netweight_kg)
                   ) %>%
                     group_by(reporter_iso, partner, date) %>%
                     summarise(trade_value_usd=sum(trade_value_usd, na.rm=T),
                               netweight_kg=sum(netweight_kg, na.rm=T)) %>%
                     mutate(commodity="natural_gas")))
       }

       if(r %in% names(countries_impose_pipeline_share)){

         pipeline_share <- countries_impose_pipeline_share[[r]]
         df <- bind_rows(df %>% filter(commodity=="gas_all"),
                   df %>% filter(commodity=="gas_all") %>%
                     mutate(trade_value_usd=trade_value_usd * pipeline_share,
                            netweight_kg=netweight_kg*pipeline_share,
                            commodity="natural_gas"),
                   df %>% filter(commodity=="lng") %>%
                     mutate(trade_value_usd=trade_value_usd * pipeline_share,
                            netweight_kg=netweight_kg*pipeline_share,
                            commodity="lng"))

       }

       if(r %in% countries_split_is_correct){
         return(df)
       }


       return(df)
     }) %>%
     filter(commodity != "gas_all")


     # totgas <- sum(df$trade_value_usd[df$commodity=='gas_all'])
     # pipe_lng <- sum(df$trade_value_usd[df$commodity %in% c('natural_gas', 'lng')])
     # to_excl = ifelse(!is.na(pipe_lng) & (pipe_lng >= totgas), 'gas_all', 'natural_gas|lng')
     # df %>% filter(!grepl(to_excl, commodity))

   ru_adjusted %<>% mutate(EU = (reporter_iso %in% codelist$iso2c[codelist$eu28=="EU"]) & (reporter_iso != 'GB'))
   ru_adjusted %<>% filter(EU) %>%
     group_by(partner, commodity, date) %>% sum_q %>%
     mutate(reporter='EU', reporter_iso='EUU') %>%
     bind_rows(ru_adjusted %>% filter(reporter_iso!='EUU'))

   # ru_adjusted %>%
   #   group_by(reporter, year=lubridate::year(date)) %>% sum_q() %>%
   #   group_by(reporter) %>% sum_q(mean) %>%
   #   arrange(desc(trade_value_usd)) -> top_importers_from_russia
   #
   # ru_adjusted %>%
   #   group_by(reporter, year=lubridate::year(date), commodity) %>% sum_q() %>%
   #   group_by(reporter, commodity) %>% sum_q(mean) %>%
   #   ggplot(aes(reporter, trade_value_usd/1e9/1.1, fill=commodity)) + geom_col() + coord_flip() +
   #   rcrea::scale_fill_crea_d('dramatic', col.index = c(1:7)) +
   #   scale_x_discrete(limits=top_importers_from_russia$reporter[1:20] %>% rev) +
   #   theme_crea() +
   #   scale_y_continuous(expand=expansion(mult=c(0,.05))) +
   #   labs(title='Largest importers of fossil fuels from Russia', x='', y='bln EUR/year',
   #        subtitle='2019–2021 average')


   flows <- ru_adjusted %>%
     ungroup() %>%
     mutate(partner=ifelse(grepl("Russia",partner), "Russia", partner),
            value_tonne=netweight_kg/1000,
            value_eur=trade_value_usd/1.1
     ) %>%
     select(country=reporter,
            partner,
            date,
            commodity,
            value_eur,
            value_tonne,
            EU
     ) %>%
     tidyr::pivot_longer(cols=c(value_eur, value_tonne),
                         names_to="unit",
                         names_prefix="value_") %>%
     mutate(source="comtrade_eurostat") %>%
     filter(!is.na(date))

   saveRDS(flows, f_cache)
   saveRDS(flows, "inst/extdata/comtrade_eurostat.RDS")

   # Export for ebc
   flows <- readRDS("inst/extdata/comtrade_eurostat.RDS")
   flows %>%
     filter(lubridate::year(date) %in% seq(2019,2021)) %>%
     mutate(commodity = case_when(grepl('oil', commodity)~'oil',
                                  grepl('coal', commodity)~'coal',
                                  grepl('natural_gas|lng', commodity)~'gas',
                                  T~"NA")) %>%
     mutate(iso3=countrycode(country, "country.name","iso3c")) %>%
     filter((country=="EU") | (iso3 %in% codelist$iso3c["NA" != codelist$eu28])) %>%
     group_by(country, partner, year=lubridate::year(date), commodity, unit, source) %>%
     summarise(value=sum(value, na.rm=T)) %>%
     group_by(country, partner, commodity, unit, source) %>%
     summarise(value = mean(value, na.rm=T)) %>%
     tidyr::pivot_wider(names_from="unit", names_prefix="value_") %>%
     mutate(value_mn_eur = sum(value_eur, na.rm=T) / 1e6) %>%
     ungroup() %>%
     select(-c(value_eur, value_tonne)) %>%
     filter(commodity !="NA") %>%
     tidyr::spread(commodity, value_mn_eur) %>%
     rowwise() %>%
     mutate(total=sum(coal,gas,oil,na.rm=T),
            unit="million eur") %>%
     arrange(desc(total)) %>%
     write_csv("results/import_from_russia_mn_eur.csv")

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



