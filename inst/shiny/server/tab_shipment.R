


# Event Observers --------------------------------------



# Download Handlers ----------------------------------

# Downloadable csv of selected dataset
output$download_shipments_csv <- downloadHandler(
  filename = function() {
    sprintf("shipments.csv")
  },
  content = function(file) {
    f <- shipments()
    write.csv(f, file, row.names = FALSE)
  }
)

# Output Elements --------------------------------------


# output$selectUnit <- renderUI({
#   # req(flows())
#   # req(input$commodity)
#   # available_units <- unique(flows() %>% filter(commodity==input$commodity) %>% pull(unit))
#   available_units <- c("MWh/day"="MWh/day", "m3/day"="m3")
#   selectInput("unit", "Unit:", choices=available_units, selected=available_units[1])
# })

shipments <- reactive({
  shipment.get_shipments()
})

# # Reactive Elements --------------------------------------




output$plot_shipments <- renderPlotly({


  shipments <- shipments()
  # commodity <- input$commodity
  # req(source, flows, commodity, unit)
  req(shipments)

  commodities <- c("lng"="LNG",
                   "crude_oil"="Crude Oil",
                   "oil_products"="Oil Products",
                   "oil_or_chemical"="Oil or Chemical")

  d <- shipments %>%
    mutate(quantity = ifelse(unit=="tonne", quantity/1000, quantity),
      unit = ifelse(unit=="tonne", "thousand tonne", unit)) %>%
    mutate(commodity = recode(commodity, !!!commodities)) %>%
    mutate(commodity = sprintf("%s - %s", commodity, unit),
           quantity = quantity)



  d["arrival_region"] = d$arrival_country
  d[d$arrival_iso2 %in% codelist$iso2c[codelist$eu28=="EU"], "arrival_region"] <- "EU28"

  identified_regions <- c("EU28", "Japan","China","United States")
  d[!d$arrival_region %in% identified_regions, "arrival_region"] <- "Other"

  add_trailing <- function(x) paste0(x, "   ")  # Tweak to prevent cut legend...
  d$arrival_region <- factor(add_trailing(d$arrival_region), levels=rev(add_trailing(c(identified_regions,"Other"))))
  d <- d %>%
    group_by(arrival_region, arrival_date_utc, commodity, unit) %>%
    summarise(quantity=sum(quantity, na.rm=T))



  colourCount = length(unique(d$arrival_region))
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))


  plt <- ggplot(d) +
    geom_bar(aes(arrival_date_utc, quantity, fill=arrival_region),
             stat="identity",
             show.legend = F) +
    facet_wrap(~commodity, scales="free_y", ncol = 1) +
    rcrea::theme_crea() +
    scale_fill_manual(values = getPalette(colourCount),
                      name=NULL) +
    scale_y_continuous(limits=c(0, NA), expand=expansion(mult=c(0, 0.1))) +
    scale_x_date(date_labels = "%b %Y", limits=c(as.Date("2022-01-01"), NA)) +
    # theme(legend.position='none') +
    labs(x="Arrival date", y=NULL)

  plt <- ggplotly(plt) %>%
    config(displayModeBar = F)
  return(plt)
})
