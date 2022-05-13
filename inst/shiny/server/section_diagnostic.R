
flows <- reactive({
 read_csv("https://api.russiafossiltracker.com/v0/overland?format=csv&date_from=2020-01-01")
})


prices <- reactive({
  read_csv("https://api.russiafossiltracker.com/v0/price?format=csv&date_from=2020-01-01")
})


gazprom_flows <- reactive({
  gazprom.get_flows() %>%
    filter(unit=='m3') %>%
    rename(value_m3 = value)})


flows_iea <- reactive({
  iea.get_flows() %>%
    filter(unit=='MWh/month') %>%
    mutate(value_m3=value * 1000 / gcv_kWh_per_m3) %>%
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
           grepl("Russia|Belarus|Turkey", departure_country)) %>%
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
  d %>%
    ungroup() %>%
    left_join(
      d %>%
        ungroup() %>%
        select(source,
               departure_country=destination_country,
               destination_country=departure_country,
               value_m3_opposite=value_m3) %>%
        filter(departure_country!=destination_country), #We keep production
      by=c("source", "destination_country", "departure_country")
    ) %>%
    mutate(value_m3_opposite=tidyr::replace_na(value_m3_opposite, 0)) %>%
    mutate(value=pmax(0, value_m3-value_m3_opposite)) %>%
    select(-c(value_m3_opposite))
}


output$plot_flows_comparison <- renderPlotly({

  year <- 2020
  top_n <- 27

  flows <- flows()
  flows_iea <- flows_iea()
  flows_bp <- flows_bp()
  req(flows, flows_iea, flows_bp)

  manual <- consumption_manual() %>%
    filter(!is.na(country))

  d <- bind_rows(
    flows %>% mutate(source='CREA'),
    flows_iea %>% mutate(source='IEA'),
    flows_bp %>% mutate(source='BP')) %>%
    filter(commodity=='natural_gas') %>%
    mutate(destination_iso2=countrycode::countrycode(destination_country, "country.name", "iso2c",
                                                     custom_match = list(`Other EU`="EU"))) %>%
    mutate(eu28 = destination_iso2 %in% c("EU", countrycode::codelist$iso2c[which(countrycode::codelist$eu28=="EU")])) %>%

    mutate(departure_country=ifelse(departure_country==destination_country, "Domestic production", departure_country)) %>%
    # filter(eu28,grepl("Russia|Belarus|Turkey", departure_country)) %>%
    group_by(departure_country, destination_country, year=lubridate::year(date), source) %>%
    summarise_at(c("value_m3", "value_eur"), sum, na.rm=T) %>%
    filter(year==!!year)


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
    summarise(value_m3=sum(value_m3)) %>%
    # netize_data() %>%
    filter(destination_country %in% head(sorted_importers, top_n)) %>%
    filter(destination_country != departure_country)

  data_plt

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
    geom_line(data=manual %>% filter(year==!!year,
                                     country %in% head(sorted_importers, top_n)) %>%
                rename(destination_country=country) %>%
                tidyr::crossing(source=unique(data_plt$source)) %>%
                mutate(legend='Consumption',
                       destination_country=factor(destination_country, head(sorted_importers, top_n))),
              inherit.aes = F,
              aes(x=source, y=value_m3/1e9, group=destination_country, col=legend)) +
    scale_color_manual(values=list(Consumption='red')) +
    scale_fill_manual(values = getPalette(colourCount), name=NULL) +
    labs(y='bcm', x=NULL) +
    facet_wrap(~destination_country,
               nrow=3) +
    rcrea::theme_crea() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme(legend.position='none')

  # Add manual points
  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))
  return(plt)


})
