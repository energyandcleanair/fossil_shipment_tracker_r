shipments <- reactive({
  utils.read_csv("https://api.russiafossiltracker.com/v0/voyage?aggregate_by=departure_country,destination_country,arrival_date,commodity&format=csv&date_from=2020-01-01")
})


flows <- reactive({
  utils.read_csv("https://api.russiafossiltracker.com/v0/overland?format=csv&date_from=2020-01-01")
})

flows_entsog <- reactive({
  utils.read_csv("https://api.russiafossiltracker.com/v0/entsogflow?format=csv&date_from=2020-01-01")
})

prices <- reactive({
  utils.read_csv("https://api.russiafossiltracker.com/v0/price?format=csv&date_from=2020-01-01")
})


gazprom_flows <- reactive({
  gazprom.get_flows() %>%
    filter(unit=='m3') %>%
    rename(value_m3 = value)})


flows_iea <- reactive({
  iea.get_flows() %>%
    select(departure_country=partner,
           destination_country=country,
           commodity,
           value_m3,
           date=month)
})



flows_bp <-  reactive({
  bp.get_flows() %>%
    filter(unit=='m3') %>%
    mutate(date=as.Date(paste0(year,"-01-01"))) %>%
    select(departure_country=partner,
          destination_country=country,
          commodity,
          value_m3=value,
          date) %>%
    mutate(departure_country=recode(departure_country, `Russian Federation`='Russia'))
})


flows_eurostat <- reactive({
  readRDS(system.file("extdata", "eurostat.RDS", package="russiacounter"))
})


flows_comtrade_eurostat <-  reactive({
  comtrade_eurostat.get_flows(use_cache=T) %>%
    filter(unit=='m3') %>%
    mutate(date=as.Date(paste0(year,"-01-01"))) %>%
    select(departure_country=partner,
           destination_country=country,
           commodity,
           value_m3=value,
           date) %>%
    mutate(departure_country=recode(departure_country, `Russian Federation`='Russia'))
})



consumption_manual <- reactive({
  # tibble(
  #   country=c("Netherlands", "Germany"),
  #   value_m3=c(21.6e9,        ),
  #   year=c(2020)
  # )
  bp.get_gas_consumption() %>%
    filter(unit=='m3') %>%
    rename(value_m3=value)
})


payments_detailed <- reactive({
  url <- sprintf("%s/v0/counter?date_from=2022-01-01&format=csv", base_url)
  payments_detailed <- utils.read_csv(url)
  return(payments_detailed)
})

counter_last <- reactive({
  url <- sprintf("%s/v0/counter_last?format=csv", base_url)
  counter_last <- utils.read_csv(url)
  return(counter_last)
})


output$plot_eurostat_lng <- renderPlotly({

  eurostat <- flows_eurostat()
  shipments <- shipments()
  req(eurostat, shipments)

  commodities <- c("lng")

  d <- bind_rows(
    eurostat %>%
      filter(commodity %in% commodities,
             unit=='m3',
             partner=='Russia') %>%
      group_by(date=lubridate::floor_date(date, 'month'),
               country) %>%
      summarise(value=sum(value, na.rm=T)*0.001626016,#m3 NG to m3 LNG
                source='Eurostat'),

    shipments %>%
      filter(commodity %in% commodities,
             departure_country=='Russia') %>%
      group_by(date=lubridate::floor_date(arrival_date, 'month'),
               country=destination_country) %>%
      summarise(value=sum(value_m3, na.rm=T),
                source='CREA')) %>%
    filter(date >= '2021-12-01',
           country %in% c('Belgium', 'Netherlands', 'Spain')) %>%
    ungroup() %>%
    tidyr::complete(date, country, source)


  colourCount = length(unique(d$source))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  plt <- ggplot(d) +
    geom_bar(aes(date, value/1e6, fill=source),
             stat="identity",position='dodge') +
    facet_wrap(~country) +
    rcrea::theme_crea() +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    scale_x_datetime(date_labels = "%b %Y") +
    scale_fill_manual(values = getPalette(colourCount), name=NULL) +
    labs(x=NULL,
         y='million m3 (LNG)')

  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))
  return(plt)


})


output$plot_payments_detailed <- renderPlotly({

  p <- payments_detailed()
  req(p)

  d <- p %>%
    group_by(date, destination_region, commodity) %>%
    summarise_at(c("value_tonne", "value_eur"),
                 sum, na.rm=T) %>%
    rcrea::utils.running_average(7, vars_to_avg = c("value_tonne", "value_eur"))

  colourCount = length(unique(p$commodity))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  plt <- ggplot(d) +
    geom_line(aes(date, value_eur/1e6, col=commodity),
              stat="identity") +
    facet_wrap(~destination_region, nrow=3, scales='free_y') +
    rcrea::theme_crea() +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    scale_x_date(date_labels = "%b %Y", limits=c(as.Date("2022-01-15"), max(p$date))) +
    scale_color_manual(values = getPalette(colourCount), name=NULL) +
    labs(x=NULL,
         y=NULL)

  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))
  return(plt)


})


output$plot_pipeline_from_russia_ts <- renderPlotly({

  flows <- flows()
  req(flows)

  d <- flows %>%
    ungroup() %>%
    filter(commodity=='natural_gas') %>%
    filter(departure_iso2 %in% c('RU', 'BY', 'TR')) %>%
    group_by(destination_country, date) %>%
    summarise(value_mcm=sum(value_m3)/1e6, na.rm=T) %>%
    rcrea::utils.running_average(7, min_values=7, vars_to_avg = "value_mcm")

  colourCount = length(unique(d$destination_country))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  plt <- ggplot(d) +
    geom_line(aes(date, value_mcm, col=destination_country)) +
    rcrea::theme_crea() +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    scale_x_date(date_labels = "%b %Y") +
    scale_color_manual(values = getPalette(colourCount), name=NULL) +
    labs(x=NULL,
         y="million m3") +
    theme(legend.position = 'none')

  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F)

  return(plt)
})


output$plot_prices <- renderPlotly({

  prices <- prices()
  req(prices)

  d <- prices %>%
    filter(country_iso2=='DE')

  colourCount = length(unique(d$commodity))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  plt <- ggplot(d) +
    geom_line(aes(date, eur_per_tonne, col=commodity)) +
    rcrea::theme_crea() +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    scale_x_date(date_labels = "%b %Y") +
    scale_color_manual(values = getPalette(colourCount), name=NULL) +
    labs(x=NULL,
         y="EUR / tonne")

  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))

  return(plt)
})




output$plot_pipelined_gas_yearly <- renderPlotly({

  flows <- flows()
  flows_iea <- flows_iea()
  flows_bp <- flows_bp()
  req(flows, flows_iea, flows_bp)


  d <- bind_rows(
    flows %>% mutate(source='CREA'),
    flows_iea %>% mutate(source='IEA'),
    flows_bp %>% mutate(source='BP')) %>%
    mutate(destination_iso2=countrycode::countrycode(destination_country, "country.name", "iso2c",
                                                   custom_match = list(`Other EU`="EU"))) %>%
    mutate(eu28 = destination_iso2 %in% c("EU", countrycode::codelist$iso2c[which(countrycode::codelist$eu28=="EU")])) %>%

    # mutate(departure_country=ifelse(departure_country==destination_country, departure_country, "Domestic production")) %>%
    filter(eu28,
           grepl("Russia|Belarus|Turkey", departure_country),
           commodity=='natural_gas',
           lubridate::floor_date(date,'month') <= lubridate::floor_date(max(flows_iea$date),'month')) %>%
    group_by(departure_country, year=lubridate::year(date), source) %>%
    summarise_at(c("value_m3", "value_eur"), sum, na.rm=T) %>%
    filter(year >=2020)

  colourCount = length(unique(d$departure_country))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  plt <- ggplot(d) +
    geom_bar(stat="identity",
             aes(source, value_m3/1e9, fill=departure_country)) +
    facet_wrap(~year) +
    rcrea::theme_crea() +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    # scale_x_date(date_labels = "%b %Y", limits=c(as.Date("2022-01-15"), max(p$date) - lubridate::days(5))) +
    scale_fill_manual(values = getPalette(colourCount), name=NULL) +
    labs(x=NULL,
         y='bcm') +
    theme(legend.position = 'none')

  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))
  return(plt)
})


netize <- function(d){

  # First group by country x partner
  d_sum <- d %>%
    ungroup() %>%
    group_by(source, departure_country, destination_country) %>%
    summarise(value_m3=sum(value_m3, na.rm=T)) %>%
    ungroup() %>%
    tidyr::complete(source, departure_country, destination_country, fill=list(value_m3=0))

  d_sum %>%
    left_join(
      d_sum %>%
        select(source,
               departure_country=destination_country,
               destination_country=departure_country,
               value_m3_opposite=value_m3) %>%
        filter(departure_country!=destination_country), #We keep production
      by=c("source", "destination_country", "departure_country")
    ) %>%
    mutate(value_m3_opposite=tidyr::replace_na(value_m3_opposite, 0)) %>%
    # mutate(value_net=pmax(0, value_m3-value_m3_opposite)) %>%
    mutate(value_m3=value_m3-value_m3_opposite) %>%
    select(-c(value_m3_opposite))
}


build_plot_flows_comparison <- function(year=2020){
  top_n <- 27

  flows <- flows()
  flows_entsog <- flows_entsog()
  flows_iea <- flows_iea()
  flows_bp <- flows_bp()
  req(flows, flows_iea, flows_bp, flows_entsog)

  manual <- consumption_manual() %>%
    filter(!is.na(country))

  d <- bind_rows(
    flows_entsog %>% mutate(source='ENTSOG (physical)'),
    flows %>% mutate(source='CREA'),
    flows_iea %>% mutate(source='IEA'),
    flows_bp %>% mutate(source='BP')) %>%
    filter(commodity=='natural_gas',
           lubridate::year(date)==!!year,
           lubridate::floor_date(date, 'month') <=  lubridate::floor_date(max(flows_iea$date), 'month')) %>%
    mutate(destination_iso2=countrycode::countrycode(destination_country, "country.name", "iso2c",
                                                     custom_match = list(`Other EU`="EU"))) %>%
    mutate(eu28 = destination_iso2 %in% c("EU", countrycode::codelist$iso2c[which(countrycode::codelist$eu28=="EU")])) %>%

    mutate(departure_country=ifelse(departure_country==destination_country, "Domestic production", departure_country)) %>%
    # filter(eu28,grepl("Russia|Belarus|Turkey", departure_country)) %>%
    group_by(departure_country, destination_country, year=lubridate::year(date), source) %>%
    summarise_at(c("value_m3", "value_eur"), sum, na.rm=T)


  greps <- c("Slovak"="Slovakia",
             "Maced"="Macedonia",
             "Czech"="Czechia",
             "Luxemb"="Luxemburg",
             "Macedo"="Macedonia",
             "Liquefied"="LNG",
             "Russia"="Russia"
  )

  for(grep in names(greps)){
    d[grepl(grep, d$departure_country),"departure_country"] = greps[[grep]]
    d[grepl(grep, d$destination_country),"destination_country"] = greps[[grep]]
  }


  sorted_importers <- d %>%
    filter(source=='CREA') %>%
    group_by(destination_country) %>%
    summarise(value_m3=sum(value_m3)) %>%
    arrange(desc(value_m3)) %>%
    # filter(!destination_country %in% c("Total exports", "Other EU"))
    pull(destination_country)

  data_plt <- d %>%
    # filter(lubridate::year(month)==2020) %>%
    group_by(departure_country, destination_country, source) %>%
    summarise(value_m3=sum(value_m3, na.rm=T)) %>%
    # netize_data() %>%
    filter(destination_country %in% head(sorted_importers, top_n)) %>%
    filter(destination_country != departure_country) %>%
    netize()

  colourCount = length(unique(d$departure_country))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))




  plt <- data_plt %>%
    ungroup() %>%
    mutate(destination_country=factor(destination_country, head(sorted_importers, top_n)),
           # source=factor(source, source_levels)
    ) %>%
    # filter(value>0) %>%
    tidyr::complete(
      departure_country, destination_country, source
    ) %>%
    ggplot() +
    geom_bar(stat="identity",
             aes(x=source,
                 y=value_m3/1e9,
                 fill=departure_country),
             show.legend = F) +
    scale_color_manual(values=c(Consumption='red')) +
    scale_fill_manual(values = getPalette(colourCount), name=NULL) +
    labs(y='bcm', x=NULL) +
    facet_wrap(~destination_country,
               nrow=3) +
    rcrea::theme_crea() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position='none')

  if(year==2020){
    plt <- plt + geom_line(data=manual %>% filter(year==!!year,
                                     country %in% head(sorted_importers, top_n)) %>%
                rename(destination_country=country) %>%
                tidyr::crossing(source=unique(data_plt$source)) %>%
                mutate(legend='Consumption',
                       destination_country=factor(destination_country, head(sorted_importers, top_n))),
              inherit.aes = F,
              aes(x=source, y=value_m3/1e9, group=destination_country, col=legend))
  }

  # Add manual points
  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))
  return(plt)
}


output$plot_flows_comparison2020 <- renderPlotly({
  build_plot_flows_comparison(year=2020)
})

output$plot_flows_comparison2021 <- renderPlotly({
  build_plot_flows_comparison(year=2021)
})

output$plot_flows_comparison2022 <- renderPlotly({
  build_plot_flows_comparison(year=2022)
})



output$plot_monthly_oil <- renderPlotly({
  # Moscow earned roughly $20 billion each month in 2022 from combined sales of crude and products amounting to about 8 million barrels a day, the Paris-based IEA said in its monthly market report

  d <- payments_detailed()
  req(d)

  eur_per_usd <- price.eur_per_usd(date_from=min(d$date),
                                   date_to=max(d$date))

  d_oil <- d %>% filter(date>='2022-01-01',
               commodity_group=='oil') %>%
    left_join(eur_per_usd) %>%
    mutate(value_usd=value_eur / eur_per_usd)


  d_oil_world <- d_oil %>%
    group_by(commodity, date=lubridate::floor_date(date, 'month')) %>%
    summarise(value_usd=sum(value_usd, na.rm=T))

  d_oil_eu <- d_oil %>%
    filter(destination_region=='EU28') %>%
    group_by(commodity, date=lubridate::floor_date(date, 'month')) %>%
    summarise(value_usd=sum(value_usd, na.rm=T))

  d_eu_share <- d_oil_eu %>%
    left_join(d_oil_world, by=c("date", "commodity"),
              suffix=c("_eu","_world")) %>%
    group_by(date) %>%
    summarise_if(is.numeric, sum) %>%
    mutate(share = value_usd_eu / value_usd_world)

  colourCount = length(unique(d_oil$commodity))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))


  plt <- ggplot(bind_rows(d_oil_world %>% mutate(region='World'),
                          d_oil_eu %>% mutate(region='EU28'))) +
    geom_bar(aes(date, value_usd/1e9, fill=commodity),
             stat='identity') +
    scale_fill_manual(values = getPalette(colourCount), name=NULL) +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    facet_wrap(~region) +
    labs(y='billion USD', x=NULL) +
    rcrea::theme_crea() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position='none') +
    geom_text(data=bind_rows(
      d_oil_world %>%
        group_by(date) %>%
        summarise(value_usd=sum(value_usd)) %>%
        mutate(region='World',
               y=value_usd + .6e9,
               label=sprintf("USD%.1fbn", value_usd/1e9)),
      d_eu_share %>%
        mutate(y=value_usd_eu + .6e9,
               label=scales::percent(share, 1),
               region='EU28')
      ),
              inherit.aes = F,
              aes(date, y/1e9, label=label))

  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))

    return(plt)

})


output$plot_counter_comparison <- renderPlotly({

  d_payments <- payments_detailed()
  d_counter_last <- counter_last()
  req(d_payments, d_counter_last)



  d <- bind_rows(
    d_payments %>%
      filter(date>='2022-02-24',
             date <= lubridate::today()) %>%
      group_by(destination_region, commodity) %>%
      summarise(value_eur=sum(value_eur, na.rm = T)) %>%
      mutate(source='payments'),
    d_counter_last %>%
      filter(commodity!='total') %>%
      group_by(destination_region, commodity) %>%
      summarise(value_eur=sum(total_eur, na.rm = T)) %>%
      mutate(source='counter'))

  colourCount = length(unique(d$commodity))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))


  plt <- ggplot(d) +
    geom_bar(aes(source, value_eur/1e9, fill=commodity),
             stat='identity') +
    scale_fill_manual(values = getPalette(colourCount), name=NULL) +
    labs(y='billion USD', x=NULL) +
    facet_wrap(~destination_region) +
    rcrea::theme_crea() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position='none') +
    geom_text(data=d %>% group_by(destination_region, source) %>% summarise(value_eur=sum(value_eur)),
              inherit.aes = F,
              aes(source, value_eur/1e9 + 5, label=sprintf("%.1f", value_eur/1e9)))

  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))

  return(plt)

})
