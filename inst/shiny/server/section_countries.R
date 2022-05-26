
country_counterlast <- reactive({
  url <- sprintf("%s/v0/counter_last?format=csv&aggregate_by=destination_iso2,commodity_group", base_url)
  country_counterlast <- utils.read_csv(url)
  return(country_counterlast)
})


output$plot_topimporters <- renderPlotly({

  p <- country_counterlast()
  req(p)

  # levels <- c("EU28", "China", "Turkey", "South Korea", "United States", "Others")
  # p <- p %>% mutate(destination_region=ifelse(destination_region %in% levels,
  #                                        destination_region,
  #                                        "Others")) %>%
  #   mutate(destination_region = factor(destination_region, levels=levels)) %>%
  #   group_by(destination_region, commodity_group, date) %>%
  #   summarise_at(c("value_tonne", "value_eur"), sum, na.rm=T)


  # colourCount = length(unique(p$commodity_group))
  # getPalette = colorRampPalette(brewer.pal(12, "Paired"))
  p <- p %>%
    mutate(
      million_eur=total_eur/1e6,
      country=destination_country)

  top_n <- 20
  top_importers <- p %>%
    filter(country != 'For orders',
           country != 'total') %>%
    group_by(country) %>%
    summarise(million_eur=sum(million_eur, na.rm=T)) %>%
    arrange(desc(million_eur)) %>%
    head(top_n) %>%
    select(country, million_eur)

  plt <- ggplot(p %>%
                  filter(country %in% top_importers$country) %>%
                  mutate(country=factor(country, levels=rev(top_importers$country)),
                         commodity_group=factor(commodity_group, levels=c("coal", "gas", "oil"),
                                                labels=c("Coal", "Fossil gas", "Oil")),
                         label=sprintf("%s<br>%s million EUR", commodity_group, scales::comma(million_eur,1))
                         ),
         aes(million_eur, country, text=label)) +
    geom_bar(aes(fill=commodity_group),
              stat="identity") +
    geom_text(data=top_importers,
              aes(million_eur, country, label=paste0("   ", scales::comma(round(million_eur), 1))),
              hjust=-1,
              inherit.aes = F,
              size=3) +
    rcrea::theme_crea() +
    scale_fill_manual(values=unname(crea_palettes$dramatic[c(2,6,3)]), name=NULL) +
    scale_x_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1)),
                       labels=scales::comma) +
    labs(x="million EUR",
         y=NULL,
         caption="Source: CREA analysis.")

  reverse_legend_labels <- function(plotly_plot) {
    n_labels <- length(plotly_plot$x$data)
    plotly_plot$x$data[1:n_labels] <- plotly_plot$x$data[n_labels:1]
    plotly_plot
  }

  plt <- ggplotly(plt, tooltip="text") %>%
    plotly::config(displayModeBar = F) %>%
    style(textposition = "right") %>%
    plotly::layout(legend = list(orientation = "h", x=0.4, y = -0.2)) %>%
    reverse_legend_labels()
  return(plt)
})
