commodity_colors_monthly <- list('Crude oil'='#35416C',
                         'Oil products'='#990000',
                         'Coal'='#333333',
                         'LNG'='#f6b26b',
                         'Pipeline gas'='#741b47')

commodity_name_monthly <- list('crude_oil'='Crude oil',
                       'crude_oil_rail_road'='Crude oil',
                       'pipeline_oil'='Crude oil',

                       'oil_products'='Oil products',
                       'oil_products_pipeline'='Oil products',
                       'oil_or_chemical'='Oil products',
                       'oil_products_rail_road'='Oil products',
                       'lpg'='Oil products',

                       'coal'='Coal',
                       'coal_rail_road'='Coal',
                       'coke_rail_road'='Coal',

                       'lng'='LNG',
                       'natural_gas'='Pipeline gas')

counter_monthly <- reactive({
  url <- sprintf("%s/v0/counter?date_from=2022-01-01&format=csv&aggregate_by=date,destination_region,commodity&use_eu=True&date_to=-14", base_url)
  counter_raw <- utils.read_csv(url)

  date_to <- max(counter_raw$date)
  last_month_str <- strftime(date_to, "%b\n1-%d")

  counter <- counter_raw %>%
    filter(commodity %in% names(commodity_name_monthly)) %>%
    mutate(commodity=factor(recode(commodity, !!!commodity_name_monthly), levels=rev(names(commodity_colors_monthly))))

  counter$month <-  lubridate::floor_date(counter$date, 'month')
  months <- sort(unique(counter$month)) %>% as.character()
  months_str = ifelse(months != lubridate::floor_date(as.Date(date_to), 'month'),
                      strftime(as.Date(months), '%b'),
                      last_month_str)
  counter$month <- factor(counter$month, levels=months, labels=months_str)

  return(counter)
})


output$plot_monthly <- renderPlotly({

  data <- counter_monthly()
  req(data)

  global_month <- data %>%
    group_by(commodity, month, date) %>%
    summarise(value_eur=sum(value_eur, na.rm=T)) %>%
    group_by(commodity, month) %>%
    summarise(value_eur=mean(value_eur, na.rm=T))


  global_month_on_month <- global_month %>%
    group_by(month) %>%
    summarise(value_eur=sum(value_eur, na.rm=T)) %>%
    arrange(month) %>%
    mutate(month_on_month=(value_eur-lag(value_eur))/lag(value_eur)) %>%
    mutate(month_on_month_str =ifelse(is.na(month_on_month) | is.infinite(month_on_month), "", sprintf("%+d%%", as.integer(round(month_on_month * 100)))))


  plt <- ggplot(global_month, aes(month, value_eur / 1e6, fill=commodity)) +

    geom_bar(stat='identity') +
    geom_text(data=global_month_on_month,
              size=4,
              color='#666666',
              inherit.aes = F,
              aes(month, value_eur / 1e6 + 20, label=month_on_month_str),
              vjust=-0.5) +
    rcrea::theme_crea() +
    labs(caption=sprintf("Source: CREA analysis."),
         y="mn EUR / day",
         x=NULL) +
    scale_y_continuous(limits=c(0, NA), expand = expansion(mult=c(0, 0.1))) +
    scale_fill_manual(values=commodity_colors_monthly,
                      guide=guide_legend(title=NULL, reverse = T))

 pltly <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))

 return(pltly)
})
