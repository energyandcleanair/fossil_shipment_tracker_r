
payments <- reactive({
  url <- sprintf("%s/v0/counter?date_from=2022-01-01&format=csv&aggregate_by=date,destination_region,commodity_group&rolling_days=7&use_eu=True", base_url)
  payments <- utils.read_csv(url)

  return(payments)
})


output$plot_payments <- renderPlotly({

  p <- payments()
  req(p)


  levels <- p %>%
    group_by(destination_region) %>%
    summarise(value_eur=sum(value_eur, na.rm=T)) %>%
    arrange(desc(value_eur)) %>%
    pull(destination_region)

  p <- p %>%
     mutate(destination_region = factor(destination_region, levels=levels))

  #   group_by(destination_region, commodity_group, date) %>%
  #   summarise_at(c("value_tonne", "value_eur"), sum, na.rm=T)

  colourCount = length(unique(p$commodity_group))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  plt <- ggplot(p) +
    geom_line(aes(date, value_eur/1e6, col=commodity_group),
              stat="identity") +
    facet_wrap(~destination_region, nrow=3) +
    rcrea::theme_crea() +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    scale_x_date(date_labels = "%b %y", limits=c(as.Date("2022-01-15"), max(p$date) - lubridate::days(5))) +
    scale_color_manual(values = getPalette(colourCount), name=NULL) +
    labs(x=NULL,
         y=NULL)

  plt <- ggplotly(plt) %>%
    plotly::config(displayModeBar = F) %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2))
  return(plt)
})
