


# Event Observers --------------------------------------

# Preset -> other select inputs
# observeEvent(input$preset,{
#
#   preset <- input$preset
#   req(preset)
#
#   if(preset=="custom"){
#     return(NULL)
#   }
#
#   params <- preset_params[[preset]]
#
#   updateSelectInput(session, "frequency",
#                     selected=params[["frequency"]])
#
#   updateSelectInput(session, "sources",
#                     selected=params[["sources"]])
#
#   updateSelectInput(session, "plot_type",
#                     selected=params[["plot_type"]])
# })

#
#
# # Only show relevant sources
# observe({
#   power_raw <- power_raw()
#   req(power_raw)
#
#   # sources <- unique(power_raw$source)
#   sources <- as.character(unique(power_raw$source))
#   updatePickerInput(session, "sources",
#                     choices=sources,
#                     selected=sources
#                     )
# })


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
output$download_csv <- downloadHandler(
  filename = function() {
    sprintf("flows_%s.csv", input$source)
  },
  content = function(file) {
    write.csv(flows(), file, row.names = FALSE)
  }
)

# Output Elements --------------------------------------

output$selectSource <- renderUI({
  available_sources <- db.available_sources()
  choices <- sources[sources %in% available_sources]
  selectInput("source", "Source:", choices=choices, selected=choices[1])
})

output$selectCommodity <- renderUI({
  req(flows())
  available_commodities <- unique(flows()$commodity)
  choices <- commodities[commodities %in% available_commodities]
  selectInput("commodity", "Commodity:", choices=choices, selected=choices[1])
})


output$selectUnit <- renderUI({
  req(flows())
  req(input$commodity)
  available_units <- unique(flows() %>% filter(commodity==input$commodity) %>% pull(unit))
  selectInput("unit", "Unit:", choices=available_units, selected=available_units[1])
})



# # Reactive Elements --------------------------------------
flows <- reactive({
  req(input$source)
  db.download_flows(source=input$source)
})

output$plot <- renderPlotly({

  # chart_type <- input$chart_type
  source <- input$source
  unit <- input$unit
  flows <- flows()
  commodity <- input$commodity
  req(source, flows, commodity, unit)

  flows <- flows %>%
    filter(commodity==!!commodity,
           unit==!!unit,
           !is.na(value),
           value>0)


  # if(source=="entsog"){
    plt <- ggplot(flows) +
      geom_line(aes(date, value, col=partner)) +
      facet_wrap(~country, scales="free_y") +
      rcrea::theme_crea() +
      scale_y_continuous(limits=c(min(0,min(flows$value)), NA), expand=expansion(mult=c(0.1, 0.1))) +
      labs(y=unit,
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

  plt <- ggplotly(plt)
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
