get_russia_oil <- function(){
  require(tidyverse); require(magrittr); require(countrycode)



# Approach 1: Comtrade ----------------------------------------------------


  library(comtradr)

  get_year_data <- function(year){
    oil_codes <- c("2709","2710")
    gas_codes <- c("2711")
    coal_codes <- c("2701","2704","2705","2706")
    q <- ct_search(partners = "Russian Federation",
              reporters = c("Germany", "Poland", "Finland", "Ukraine"), #TODO add countries
              trade_direction = "imports",
              commod_codes=c(oil_codes, gas_codes, coal_codes),
              freq="monthly",
              start_date = year,
              end_date = year) %>%
      mutate(date=strptime(paste0(period,"01"),format="%Y%m%d"))

    if(nrow(q)==0){
      return(NULL)
    }else{
      return(q)
    }
  }

  q <- lapply(seq(2017, 2022), get_year_data) %>% do.call(bind_rows, .)

  ggplot(q) +
    geom_line(aes(date, trade_value_usd, col=commodity_code)) +
    facet_wrap(~reporter)


# Approach 2: EUROSTAT ----------------------------------------------------


  # Lauri's code using eurostat
  # require(eurostat)
  # search_eurostat('solid.*fuels.*partner', fixed=F)
  # exim_codes <- search_eurostat('(Imports|Exports).*partner', fixed=F) %>% data.frame %>% filter(data.end=='2020')
  # exim <- exim_codes$code %>% lapply(get_eurostat)
  # names(exim) <- exim_codes$code
  #
  #
  #
  # exim %>% bind_rows(.id='code') %>% filter(year(time)==2019) %>% filter(values != 0) -> d
  # d %<>% left_join(exim_codes %>% select(code, title))
  # d %>% filter(values != 0) %>%
  #   mutate(values = values * ifelse(grepl('Imports', title), 1, -1),
  #          product = gsub('.* of | by .*', '', title)) %>%
  #   group_by(geo, partner, product, time) %>%
  #   summarise(value=sum(values, na.rm=T)) ->
  #   net_imports
  # net_imports %>% filter(!grepl('EU|EA19', geo), partner != 'TOTAL', !grepl('biofuels|electricity', product)) %>%
  #   group_by(partner, product) %>%
  #   summarise(across(value, sum, na.rm=T)) %>% group_by(product) %>% slice_max(value, n=5) -> top_partners
  # net_imports %>% filter(!grepl('EU|EA19', geo), !grepl('elect', product), partner != 'TOTAL', , value>0) %>%
  #   mutate(import_from=ifelse(partner %in% top_partners$partner, partner, 'others')) %>%
  #   group_by(geo, import_from, product, time) %>%
  #   summarise(across(value, sum, na.rm=T)) %>% ungroup %>%
  #   mutate(across(c(geo, import_from), countrycode, 'iso2c', 'country.name.en',
  #                 custom_match = c(NSP='not specified', OTHERS='others', EL='Greece', 'XK'='Kosovo', UK='UK'))) %>%
  #   ggplot(aes(geo, value, fill=import_from)) + geom_col() + facet_wrap(~product, scales = 'free_x') + coord_flip() +
  #   scale_x_discrete(limits=rev)
}
