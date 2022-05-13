build_price_models <- function(production=F){

  library(tidyverse)
  library(magrittr)
  library(lubridate)
  library(countrycode)

  oil_codes <- c("2709","2710")
  gas_codes <- c("2711","271121","271111")
  coal_codes <- c("2701","2704","2705","2706")

  # Collect predictors ------------------------------------------------------
  prices_monthly <- get_prices_monthly()


  # Collect prices ----------------------------------------------------------
  # Collect trade data

  # imp <- utils.collect_comtrade(partners="Russian Federation",
  #                               reporters="all",
  #                               years=seq(2016,2022),
  #                               codes=c(coal_codes, oil_codes, gas_codes),
  #                               frequency="monthly",
  #                               stop_if_no_row=F) %>%
  #   filter(trade_flow=="Imports")
  # saveRDS(imp, "cache/imp_for_building_models.RDS")
  imp <- readRDS("cache/imp_for_building_models.RDS")
  if(production){
    max_date <- "2022-01-01"
  }else{
    max_date <- "2022-01-01"
  }


  # exp <- utils.collect_comtrade(partners="all",
  #                               reporters="Russian Federation",
  #                               years=seq(2015,2019), # No record for 2020, and 2021
  #                               codes=c(coal_codes, oil_codes, gas_codes),
  #                               frequency="monthly",
  #                               stop_if_no_row=F) %>%
  #   filter(trade_flow=="Exports")
  # saveRDS(exp, "cache/exp_for_building_models.RDS")
  exp <- readRDS("cache/exp_for_building_models.RDS")

  clean_comtrade <- function(df, is_import){
    df <- df %>%
      mutate(across(is.logical, as.character),
             across(matches('_kg|_usd|year|period|flag'), as.numeric),
             across(matches('code|level'), as.character)) %>%
      mutate(date=ymd(paste0(period, '01')),
             price_usd_per_tonne=trade_value_usd/(netweight_kg/1000)) %>%
      full_join(prices_monthly, by="date") %>%
      mutate(date = as.Date(date))

    df$commodity[grep('crude$', df$commodity)] <- "crude_oil"
    df$commodity[grep('not crude', df$commodity)] <- "oil_products"
    df$commodity[grep('^Coal.*ovoids', df$commodity)] <- "coal"
    df$commodity[grep('liquefied', df$commodity)] <- "lng"
    df$commodity[grep('Coal gas', df$commodity)] <- "coal_gas"
    df$commodity[grep('gases', df$commodity)] <- "natural_gas"

    df %<>% filter(grepl('coal$|crude_oil|oil_products|lng|natural_gas', commodity))

    group_cols <- if(is_import){c("reporter", "reporter_iso")}else{c("partner", "partner_iso")}
    df <- df %>%
      group_by_at(c("commodity", "date", group_cols)) %>%
      summarise(across(c(trade_value_usd, netweight_kg), sum, na.rm=T)) %>%
      mutate(price_usd_per_tonne = trade_value_usd/(netweight_kg/1000)) %>%
      ungroup()
    return(df)
  }

  imp_cleaned <- clean_comtrade(imp, is_import=T) %>% rename(country=reporter) %>% select(-c(reporter_iso))
  exp_cleaned <- clean_comtrade(exp, is_import=F) %>% rename(country=partner) %>% select(-c(partner_iso))

  imp_cleaned <- imp_cleaned %>% filter(date <= max_date)
  exp_cleaned <- exp_cleaned %>% filter(date <= max_date)

  # plt <- imp_cleaned %>%
  #   group_by(commodity, date) %>%
  #   summarise(netweight_kg=sum(netweight_kg, na.rm=T),
  #             count=n()) %>%
  #   arrange(desc(date)) %>% ggplot() + geom_line(aes(date, netweight_kg, col=commodity))
  #
  # library(plotly)
  # ggplotly(plt)

  # exp_cleaned %>%
  #   group_by(commodity, date) %>%
  #   summarise(count=n()) %>%
  #   arrange(desc(date)) %>% View()

  # Combine: take source with max flow for that month
  trade <- bind_rows(imp_cleaned, exp_cleaned) %>%
    group_by(commodity, date, country) %>%
    dplyr::slice_max(trade_value_usd, n=1) %>%
    ungroup() %>%
    left_join(prices_monthly)

  eur_usd <- price.eur_per_usd(date_from=min(trade$date),
                               monthly=T)
  trade <- trade %>%
    left_join(eur_usd) %>%
    mutate(trade_value_eur=trade_value_usd * eur_per_usd,
           price_eur_per_tonne=price_usd_per_tonne * eur_per_usd,
    ) %>%
    select(-c(trade_value_usd, price_usd_per_tonne))

  world_price <- trade %>%
    filter(!is.na(price_eur_per_tonne),
           !is.infinite(price_eur_per_tonne)) %>%
    group_by(commodity, date) %>%
    summarise_at(c("trade_value_eur", "netweight_kg"), sum) %>%
    mutate(world_price_eur_per_tonne = trade_value_eur/(netweight_kg/1000)) %>%
    select(commodity, date, world_price_eur_per_tonne) %>%
    ungroup()

  trade %<>% left_join(world_price)

  countrytable <- trade %>% ungroup %>% distinct(country) %>%
    mutate(country_iso = countrycode(country, 'country.name.en', 'iso2c'))

  trade %<>% mutate(country_iso = countrytable$country_iso[match(country, countrytable$country)],
                    EU = country_iso %in% codelist$iso2c[!is.na(codelist$eu28)])



  # Europe ------------------------------------------------------------------
  trade_eu <- trade %>%
    filter(EU) %>%
    filter((commodity!="lng") | (price_eur_per_tonne<1500)) %>%
    filter((commodity!="oil_products") | (price_eur_per_tonne>80)) %>%
    filter(!is.na(price_eur_per_tonne) & !is.infinite(price_eur_per_tonne)) %>%
    filter(netweight_kg>100 * 1000) %>%
    group_by(commodity, date) %>%
    summarise(across(c(trade_value_eur, netweight_kg), sum, na.rm=T),
              across(all_of(c('world_price_eur_per_tonne', prices_monthly %>% select(is.numeric) %>% names)), unique)) %>%
    mutate(price_eur_per_tonne = trade_value_eur/(netweight_kg/1000))

  trade_with_predictions_eu <- trade_eu %>%
    group_by(commodity) %>%
    group_map(function(df, group) {
      print(group$commodity)
      start_year = ifelse(group$commodity=='natural_gas', 2016, ifelse(group$commodity=='lng', 2018, 2015))
      independents = case_when(group$commodity=='coal' ~ 'ara',
                               group$commodity=='natural_gas' ~
                                 'brent+ttf*date*abs(month(date)-9)',
                               group$commodity=='lng' ~ 'ttf',
                               grepl('oil', group$commodity) ~
                                 'brent + lag(brent) + lag(brent, 3) + 0',
                               T ~ 'brent + lag(price_eur_per_tonne, 12)')

        df <- df %>% arrange(date) %>% filter(year(date)>=start_year)
        m <- df %>%
        lm(as.formula(paste('price_eur_per_tonne ~', independents)), data=.)

      tibble_row(data=list(as.data.frame(df %>% mutate(predicted_price = predict(m, df)))),
           model=list(m),
           commodity=group$commodity,
           price_ceiling = max(df$price_eur_per_tonne))
    }) %>% do.call(bind_rows, .)

  trade_with_predictions_eu %>% select(-c(model)) %>%
    tidyr::unnest(data)  %>% pivot_longer(c(price_eur_per_tonne, predicted_price, price_ceiling)) %>% filter(year(date)>=2016) %>%
    ggplot(aes(date, value, col=name)) + facet_grid(~commodity) + geom_line()

  trade_with_predictions_eu %>% select(-c(model)) %>%
    tidyr::unnest(data)  %>% filter(year(date)>=2016) %>%
    ggplot(aes(price_eur_per_tonne, predicted_price)) +
    facet_wrap(~commodity, scales='free') + geom_point() + geom_abline()+ geom_smooth()

  if(production){
    suffix=''
  }else{
    suffix='_development'
  }

  saveRDS(trade_with_predictions_eu, sprintf('inst/extdata/pricing_models_eu%s.RDS',suffix))
  saveRDS(trade_with_predictions_eu, sprintf('data/pricing_models_eu%s.RDS',suffix))



  # Non-Europe --------------------------------------------------------------
  top_importers <- trade %>%
    filter(!EU, !is.na(country_iso), year(date)>=2017) %>%
    group_by(country, commodity) %>%
    summarise(across(trade_value_eur, mean), n=n()) %>%
    group_by(commodity) %>%
    mutate(share = trade_value_eur/sum(trade_value_eur)) %>%
    filter(n>10, share>.025)

  trade_grouped <- top_importers %>% select(-trade_value_eur) %>% left_join(trade)
  trade_grouped <- trade %>%
    filter(!is.na(country_iso)) %>%
    group_by(commodity, date, country = ifelse(EU, 'EU', 'others')) %>%
    summarise(across(c(trade_value_eur, netweight_kg), sum, na.rm=T),
              across(all_of(c('world_price_eur_per_tonne', prices_monthly %>% select(is.numeric) %>% names)), unique)) %>%
    mutate(price_eur_per_tonne = trade_value_eur/(netweight_kg/1000)) %>%
    bind_rows(trade_grouped)

  trade_with_predictions <- trade_grouped %>%
    group_by(commodity, country, country_iso) %>%
    group_map(function(df, group) {
      message(group)
      start_year = ifelse(group$commodity %in% c('lng', 'coal'), 2016, 2015) #Russia lng & coal prices in 2015 were silly
      message(start_year)
      max_deviation = 10
      independents = case_when(group$commodity=='coal' ~ 'ara + global_coal',
                               group$commodity=='natural_gas' ~
                                 'brent + ttf + asia_lng',
                               group$commodity=='lng' ~ 'ttf + asia_lng',
                               grepl('oil', group$commodity)  ~
                                 'brent + lag(brent) + lag(brent, 3) + 0',
                               T ~ 'brent')

      df %<>% filter(price_eur_per_tonne/world_price_eur_per_tonne < max_deviation) %>%
        filter(year(date)>=start_year)
      df %>% arrange(date) %>%
        lm(as.formula(paste('price_eur_per_tonne ~', independents)), data=.) -> m

      tibble_row(data=list(as.data.frame(df %>% mutate(predicted_price = predict(m, df)))),
                 model=list(m),
                 group,
                 price_ceiling = max(df$world_price_eur_per_tonne))
    }) %>% do.call(bind_rows, .)

  trade_with_predictions %>% select(-c(model)) %>%
    tidyr::unnest(data) ->
    trade_w_pred_df


  trade_w_pred_df %>% pivot_longer(matches('price')) %>%
    filter(year(date)>=2016, commodity=='crude_oil') %>%
    filter(name %in% c("price_eur_per_tonne", "predicted_price")) %>%
    ggplot(aes(date, value, col=name)) + facet_wrap(~country, scales='free_y') + geom_line()

  trade_w_pred_df %>% filter(year(date)>=2016) %>%
    ggplot(aes(price_eur_per_tonne, predicted_price, col=country)) +
    facet_wrap(~commodity, scales='free') + geom_point() + geom_abline()+ geom_smooth()

  trade_with_predictions_non_eu <- trade_with_predictions %>%
    filter(country != 'EU')

  saveRDS(trade_with_predictions_non_eu,
          sprintf('inst/extdata/pricing_models_noneu%s.RDS', suffix))
  saveRDS(trade_with_predictions_non_eu,
          sprintf('data/pricing_models_noneu%s.RDS',suffix))

  trade_with_predictions %>% rowwise() %>% group_split() %>%
    lapply(function(df) {
      tibble(df, date=prices_monthly$date, predicted_price = predict(df$model[[1]], prices_monthly))
    }) %>% bind_rows() %>% filter(year(date)==2022, month(date)>=2) %>%
    group_by(country, commodity, date) %>% summarise(across(predicted_price, mean)) %>% na.omit() %>%
    write_csv('data/predicted_export_prices.csv')
}
