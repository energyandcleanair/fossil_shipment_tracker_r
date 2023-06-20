


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

output$selectRunningDays <- renderUI({
  sliderInput("running_days", "Running average (days)", 1, 31, 7, 1)
})

flows <- reactive({

  # Combine pipeline and shipments
  bind_rows(
    db.download_flows(source="entsog") %>% filter(grepl("MWh",unit)),
    shipments <- shipment.get_shipments() %>%
      mutate(date=as.Date(arrival_date_utc),
             country=countrycode(arrival_iso2, "iso2c", "country.name"),
             partner=countrycode(departure_iso2, "iso2c", "country.name"),
             value=quantity,
             source="AIS") %>%
      group_by(date, country, partner, unit, commodity, source) %>%
      summarise(value=sum(value, na.rm=T),
                count=n()) %>%
      select(date, country, partner, value, unit, commodity, source)
  )

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
  # unit <- input$unit
  flows <- flows()
  running_days <- input$running_days
  # req(source, flows, commodity, unit)
  req(flows, running_days)

  # unit_label <- list(`MWh/day`="MWh / day", m3='m3 / day')[[unit]]


  commodities_rev <- as.list(names(commodities)) %>% `names<-`(commodities)
  d <- flows %>%
    filter(!is.na(value), value>0) %>%
    filter(commodity %in% c("natural_gas", "lng", "crude_oil")) %>%
    # filter(unit==!!unit) %>%
    group_by(date, source, commodity, unit) %>%
    summarise(value=sum(value)) %>%
    # tidyr::pivot_longer(cols=c(value_tonne, value_eur),
    #                     names_prefix="value_",
    #                     names_to="unit") %>%
    # filter(unit==!!unit) %>%
    mutate(commodity=recode(commodity, !!!commodities_rev)) %>%
    rcrea::utils.running_average(running_days) %>%
    mutate(commodity=sprintf("%s (%s)", commodity, unit))


  # if(source=="entsog"){
    plt <- ggplot(d) +
      geom_line(aes(date, value, col=commodity)) +
      facet_wrap(~commodity, scales="free_y", ncol=1) +
      rcrea::theme_crea() +
      rcrea::scale_color_crea_d("dramatic") +
      scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0.1, 0.1))) +
      scale_x_date(limits=c(as.Date("2022-01-01"), NA)) +
      labs(y=NULL,
           x=NULL)
  # }

  plt <- ggplotly(plt)  %>% config(displayModeBar = F)


  return(plt)
})
