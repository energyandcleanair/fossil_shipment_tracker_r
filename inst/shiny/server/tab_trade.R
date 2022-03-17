


# Event Observers --------------------------------------

# observe({
#   # Remove plotly parameters
#   url <- input$.shinyURL
#   req(url)
#   url_new <- gsub("&plotly[^&]*","", url)
#
#   if(url != url_new){
#     updateTextInput(session, ".shinyURL", value=url_new)
#   }
# })


# Download Handlers ----------------------------------

# Downloadable csv of selected dataset
output$download_trade_csv <- downloadHandler(
  filename = function() {
    "trade_with_russia.csv"
  },
  content = function(file) {
    write.csv(trade(), file, row.names = FALSE)
  }
)

# Output Elements --------------------------------------

output$selectYear <- renderUI({
  req(flows_aggregated())
  available_years <- unique(flows_aggregated()$year)
  selectInput("year", "Year:", choices=available_years, selected=max(available_years))
})

output$selectUnit <- renderUI({
  req(flows_aggregated())
  available_units <- unique(flows_aggregated()$unit)
  selectInput("unit", "Unit:", choices=available_units, selected=available_units[1])
})

output$selectCommodity <- renderUI({
  req(flows_aggregated())
  available_commodities <- unique(flows_aggregated()$commodity)
  selectInput("commodity", "Commodities:", multiple=T, choices=available_commodities,
              selected=intersect(c("coal","natural_gas", "lng", "gas_all", "oil"), available_commodities))
})

output$selectCountry <- renderUI({
  req(flows_aggregated())
  available_commodities <- unique(flows_aggregated()$commodity)
  selectInput("commodity", "Commodities:", multiple=T, choices=available_commodities,
              selected=intersect(c("coal","natural_gas", "lng", "gas_all", "oil"), available_commodities))
})

# # Reactive Elements --------------------------------------
trade <- reactive({
  # Lauri's mix of comtrade and eurostat
  # db.download_flows(source="comtrade_eurostat_russia")
  readRDS(system.file("extdata","comtrade_eurostat.RDS", package="russiacounter"))
})



output$plot_trade <- renderPlotly({

  # unit <- input$unit
  # year <- input$year
  # input$commodity
  # top_n <- 10
  # direction <- "to_europe" # To implement: from_russia
  f <- trade() %>%
    filter(lubridate::year(date) %in% seq(2019,2021))

  req(f)

  f %>%
    filter(unit=="eur") %>%
    group_by(country, year=lubridate::year(date)) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    group_by(country) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    arrange(desc(value)) %>%
    head(20) %>%
    pull(country) -> top_importers_from_russia

  commodities_rev <- as.list(names(commodities)) %>% `names<-`(commodities)

  plt <- f %>%
    filter(unit=="eur",
           grepl("Russia", partner)) %>%
    group_by(country, year=lubridate::year(date), commodity, unit) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    group_by(country, commodity, unit) %>%
    summarise(value=mean(value, na.rm=T)) %>%
    mutate(commodity=recode(commodity, !!!commodities_rev),
           value=value/1e9,
           label=sprintf("%s - %s\n%.3f bn EUR",country, commodity, value)
           ) %>%
    ggplot(aes(country, value, fill=commodity, text=label)) +
    geom_col() + coord_flip() +
    rcrea::scale_fill_crea_d('dramatic', col.index = c(1:7), name=NULL) +
    scale_x_discrete(limits=top_importers_from_russia %>% rev) +
    rcrea::theme_crea() +
    scale_y_continuous(expand=expansion(mult=c(0,.05))) +
    labs(
      # title='Largest importers of fossil fuels from Russia',
      x='', y='bln EUR/year',
      subtitle='2019–2021 average')

  plt <- ggplotly(plt, tooltip = c("text")) %>%
    config(displayModeBar = F)

  # Pushing only once, to be embedded in Wordpres article
  if(F){
    readRenviron(".Renviron")
    Sys.setenv("plotly_username"=Sys.getenv("PLOTLY_API_USER"))
    Sys.setenv("plotly_api_key"=Sys.getenv("PLOTLY_API_KEY"))

    api_create(plt, filename = "russia_fossil_trade")
  }

  return(plt)




    # req(flows_aggregated(), year, unit, input$commodity)
    # f_agg <- flows_aggregated()
    #
    # f_agg <- bind_rows(f_agg,
    #                    f_agg %>%
    #                      dplyr::mutate(iso2=countrycode(country, "country.name", "iso2c")) %>%
    #                      filter(iso2 %in% russiacounter::eugb_iso2s) %>%
    #                      group_by(partner, year, unit, source, commodity) %>%
    #                      summarise(value=sum(value, na.rm=T)) %>%
    #                      mutate(country="EU + GB")) %>%
    #   filter(unit==!!unit,
    #          year==!!year,
    #          commodity %in% input$commodity)
    #
    # top_n_countries <- f_agg %>%
    #   filter(country!="Turkey") %>%
    #   group_by(country, commodity, unit) %>% summarise(value=sum(value)) %>%
    #   group_by(commodity, unit) %>%
    #   mutate(rank=rank(value)) %>%
    #   group_by(country) %>%
    #   summarise(rank=sum(rank)) %>%
    #   arrange(desc(rank)) %>%
    #   head(top_n) %>%
    #   pull(country)
    #
    # d <- f_agg %>%
    #   filter(country %in% top_n_countries)
    #
    # commodities_rev <- as.list(names(commodities)) %>% `names<-`(commodities)
    #
    # partners <- unique(d$partner)
    #
    # partners_russia_transit <- c("Russia", "Slovakia", "Ukraine", "Polant")
    # colors_russia_transit <- RColorBrewer::brewer.pal(length(partners_russia_transit), "Reds") %>%
    #   rev() %>%
    #   `names<-`(partners_russia_transit)
    #
    # partners_others <- setdiff(partners, partners_russia_transit)
    # colors_others <- mycolors <- colorRampPalette(brewer.pal(9, "Blues"))(length(partners_others)) %>%
    #   `names<-`(partners_others)
    #
    # colors <- c(colors_russia_transit, colors_others)
    # colors[["Other"]] <- "#CACACA"
    #
    # d_plot <- d %>%
    #   mutate(partner=tidyr::replace_na(partner, "Other"),
    #          partner=factor(partner, levels=names(colors)),
    #          country=factor(country, levels=top_n_countries)) %>%
    #   group_by(country, commodity, unit, source, year) %>%
    #   mutate(value=value/sum(value)*100) %>%
    #   mutate(commodity = recode(commodity, !!!commodities_rev)) %>%
    #   mutate(commodity_unit = sprintf("%s (%s)", commodity, unit)) %>%
    #   ungroup() %>%
    #   # Add label
    #   mutate(label=ifelse(value>10 & (partner %in% partners_russia_transit), sprintf("%s %.0f %%", partner, value), ""))
    #
    # d_plot <- d_plot %>%
    #   right_join( d_plot %>% tidyr::expand(country, commodity, year, unit, commodity_unit)) %>%
    #   mutate(value=tidyr::replace_na(value, 0),
    #          partner=tidyr::replace_na(partner, "Other"),
    #          label=tidyr::replace_na(label,""))
    #
    #
    # plots <- lapply(split(d_plot, d_plot$country), function(d_country){
    #   d_country %>%
    #     group_by(partner) %>%
    #     arrange(partner) %>%
    #     plot_ly(
    #       x = ~value,
    #       y = ~commodity_unit,
    #       text=~label,
    #       color= ~partner,
    #       colors=colors,
    #       # colors = 'Reds',
    #       type = 'bar',
    #       legendgroup=~partner,
    #       showlegend=F) %>%
    #    add_annotations(text = ~label,
    #                         x = ~value,
    #                         y = ~commodity_unit,
    #                    xanchor="right",
    #                         # xref = "x",
    #                         # yref = "y",
    #
    #                         font = list(family = 'Arial',
    #                                     size = 12,
    #                                     color="#FFFFFF"),
    #                         showarrow = FALSE) %>%
    #   plotly::layout(yaxis =
    #                    list(title = unique(d_country$country)),
    #            xaxis= list(title="%"))
    # })
    #
    # plt <- do.call(function(...){subplot(..., nrows=10, titleY=T, shareX=T)}, plots) %>% layout(barmode = 'stack', showlegend = TRUE)
    # return(plt)
    #
})
