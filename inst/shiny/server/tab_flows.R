


# Event Observers --------------------------------------



# Download Handlers ----------------------------------

# Downloadable csv of selected dataset
output$download_flows_csv <- downloadHandler(
  filename = function() {
    sprintf("flows.csv")
  },
  content = function(file) {
    f <- flows()
      # filter(source=="entsog") %>%
      # filter(unit=="tonne") %>%
      # select(-c(unit)) %>%
      # rename(value_tonne=value) %>%
      # mutate(value_m3=value_tonne*1000/kg_per_m3)

    write.csv(f, file, row.names = FALSE)
  }
)

# Output Elements --------------------------------------



# output$selectCommodity <- renderUI({
#   req(flows())
#   available_commodities <- unique(flows()$commodity)
#   choices <- commodities[commodities %in% available_commodities]
#   selectInput("commodity", "Commodity:", choices=choices, selected=choices[1])
# })


output$selectUnit <- renderUI({
  # req(flows())
  # req(input$commodity)
  # available_units <- unique(flows() %>% filter(commodity==input$commodity) %>% pull(unit))
  available_units <- c("MWh/day"="MWh/day", "m3/day"="m3")
  selectInput("unit", "Unit:", choices=available_units, selected=available_units[1])
})

flows <- reactive({
  # Don't load flows if this is not the tab queried
  # query <- parseQueryString(session$clientData$url_search)
  # if(!is.null(query$tab) && (query$tab != "counter")){
  #   return(NULL)
  # }
  # db.download_flows(source="combined_light")
  db.download_flows(source="entsog")
})

# # Reactive Elements --------------------------------------
# flows_combined <- reactive({
#   req(flows())
#   flows() %>%
#     filter(date >= "2022-01-01") %>%
#     mutate(commodity=recode(commodity, !!!list("crude_oil"="oil",
#                                                "oil_products"="oil"))) %>%
#     group_by(date, unit, source, commodity, transport) %>%
#     summarise_at(c("value", "value_eur"),
#                  sum, na.rm=T) %>%
#     filter(commodity=="natural_gas") %>%
#     ungroup()
# })


output$plot_flows <- renderPlotly({

  # chart_type <- input$chart_type
  # source <- input$source
  unit <- input$unit
  flows <- flows()
  # commodity <- input$commodity
  # req(source, flows, commodity, unit)
  req(flows, unit)

  unit_label <- list(`MWh/day`="MWh / day", m3='m3 / day')[[unit]]


  commodities_rev <- as.list(names(commodities)) %>% `names<-`(commodities)
  d <- flows %>%
    filter(!is.na(value), value>0) %>%
    filter(unit==!!unit) %>%
    group_by(date, source, commodity, unit) %>%
    summarise(value=sum(value)) %>%
    # tidyr::pivot_longer(cols=c(value_tonne, value_eur),
    #                     names_prefix="value_",
    #                     names_to="unit") %>%
    # filter(unit==!!unit) %>%
    mutate(commodity=recode(commodity, !!!commodities_rev))


  # if(source=="entsog"){
    plt <- ggplot(d) +
      geom_line(aes(date, value)) +
      facet_wrap(~commodity, scales="free_y") +
      rcrea::theme_crea() +
      scale_y_continuous(limits=c(min(0,min(flows$value)), NA), expand=expansion(mult=c(0.1, 0.1))) +
      labs(y=unit_label,
           x=NULL)
  # }

  # if(chart_type=="lines"){
  #   ggplot(flows) +
  #     geom_bar(stat="identity",
  #              aes(date, value, fill=source)) +
  #     facet_wrap(~country)
  #
  #     plot_ly(flows,
  #           x = ~date,
  #           y = ~value,
  #           color = ~source,
  #           customdata=~source,
  #           colors=creapower::palette_power(),
  #           type = "scatter",
  #           mode="lines+marker",
  #           hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
  #           showlegend = T) %>%
  #     layout(
  #       hovermode = "x unified",
  #       yaxis = list(title = 'Power generation (MW)'),
  #       xaxis = list(title = ''))
  # }
  #
  # if(plot_type=="area"){
  #   plt <- plot_ly(power_sources,
  #           x = ~date,
  #           y = ~output_mw,
  #           color = ~source,
  #           customdata = ~source,
  #           colors = creapower::palette_power(),
  #           type = 'scatter',
  #           mode = 'lines',
  #           line = list(width = 0),
  #           alpha = 0.9,
  #           stackgroup = 'one',
  #           hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
  #           showlegend = T) %>%
  #     layout(
  #       hovermode = "x unified",
  #       yaxis = list(title = 'Power generation (MW)'),
  #       xaxis = list(title = ''))
  # }
  #
  # if(plot_type=="area_pct"){
  #   plt <- plot_ly(power_sources,
  #           x = ~date,
  #           y = ~output_pct,
  #           color = ~factor(source),
  #           customdata = ~source,
  #           colors = creapower::palette_power(),
  #           type = 'scatter',
  #           mode = 'lines',
  #           line = list(width = 0),
  #           alpha = 0.9,
  #           stackgroup = 'one',
  #           hovertemplate = '%{customdata} %{y:.0%}<extra></extra>',
  #           showlegend = T) %>%
  #     layout(
  #       hovermode = "x unified",
  #       yaxis = list(title = 'Share of power generation',
  #                    tickformat = '.0%'),
  #       xaxis = list(title = ''))
  # }
  #
  # if(plot_type=="bar"){
  #
  #   power_deyeared <- power_sources %>%
  #     mutate(year=lubridate::year(date),
  #            date2000 = lubridate::`year<-`(date, 2000)) %>%
  #     group_by(iso2, region, date2000, year) %>%
  #     summarise(output_mw=sum(output_mw)) %>%
  #     ungroup()
  #
  #   tickformat <- recode(frequency,
  #                        "day"="%e %b",
  #                        "week"= "%W",
  #                        "month"="%b",
  #                        "year"="%Y")
  #
  #   dtick <- ifelse(frequency=="month", "M1", NA)
  #
  #   plt <- plot_ly(power_deyeared,
  #                  x = ~date2000,
  #                  y = ~output_mw,
  #                  color = ~factor(year),
  #                  customdata = ~year,
  #                  colors = 'Reds',
  #                  type = 'bar',
  #                  alpha = 0.9,
  #                  hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
  #                  showlegend = T) %>%
  #            layout(
  #              hovermode = "x unified",
  #              yaxis = list(title = 'Power generation (MW)'),
  #              xaxis = list(title = '',
  #                           dtick = dtick,
  #                           tickformat=tickformat))
  # }

  plt <- ggplotly(plt)  %>% config(displayModeBar = F)

    #
    # layout(
    #   annotations = list(x = 1, y = 0, text = caption,
    #      showarrow = F, xref='paper', yref='paper',
    #      xanchor='right', yanchor='auto', xshift=0, yshift=-60,
    #      font=list(color="#AAAAAA")),
    #   margin = list(b=60),
    #   yaxis = list(fixedrange=T))

  return(plt)
})
