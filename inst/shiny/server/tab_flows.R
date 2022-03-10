


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
output$download_csv <- downloadHandler(
  filename = function() {
    sprintf("flows_%s.csv", input$source)
  },
  content = function(file) {
    write.csv(flows(), file, row.names = FALSE)
  }
)

# Output Elements --------------------------------------

output$selectUnit <- renderUI({
  # req(flows())
  # req(input$commodity)
  # available_units <- unique(flows() %>% filter(commodity==input$commodity) %>% pull(unit))
  available_units <- c("Physical"=c("tonne","m3", "TJ"), "EUR"="eur")
  selectInput("unit", "Unit:", choices=available_units, selected=available_units[1])
})

output$selectUnit <- renderUI({
  req(flows_aggregated())
  available_years <- unique(flows_aggregated()$year)
  selectInput("year", "Year:", choices=available_years, selected=max(available_years))
})

# # Reactive Elements --------------------------------------
flows <- reactive({
  f <- db.download_flows(source="eurostat") %>%
    filter(date>="2019-01-01")
  return(f)
})


flows_aggregated <- reactive({
  req(flows())
  f <- flows()
  f_agg <- f %>%
    group_by(country, partner, year=lubridate::year(date), unit, source, commodity) %>%
    summarise(value=sum(value, na.rm=T)) %>%
    ungroup()
  return(f_agg)
})

output$plot <- renderPlotly({

  # unit <- input$unit
  year <- input$year
  top_n <- 10

  req(flows_aggregated(), year)
  f_agg <- flows_aggregated()

  f_agg <- bind_rows(f_agg,
                     f_agg %>%
                       dplyr::mutate(iso2=countrycode(country, "country.name", "iso2c")) %>%
                       filter(iso2 %in% russiacounter::eugb_iso2s) %>%
                       group_by(partner, year, unit, source, commodity) %>%
                       summarise(value=sum(value, na.rm=T)) %>%
                       mutate(country="EU + GB"))

  top_n_countries <- f_agg %>%
    filter(country!="Turkey") %>%
    filter(unit!="TJ") %>%
    filter(year==!!year) %>%
    group_by(country, commodity, unit) %>% summarise(value=sum(value)) %>%
    group_by(commodity, unit) %>%
    mutate(rank=rank(value)) %>%
    group_by(country) %>%
    summarise(rank=sum(rank)) %>%
    arrange(desc(rank)) %>%
    head(top_n) %>%
    pull(country)

  d <- f_agg %>%
    filter(year==!!year,
           unit!="TJ",
           country %in% top_n_countries)

  commodities_rev <- as.list(names(commodities)) %>% `names<-`(commodities)

  partners <- unique(d$partner)

  partners_russia_transit <- c("Russia", "Slovakia", "Ukraine", "Polant")
  colors_russia_transit <- RColorBrewer::brewer.pal(length(partners_russia_transit), "Reds") %>%
    rev() %>%
    `names<-`(partners_russia_transit)

  partners_others <- setdiff(partners, partners_russia_transit)
  colors_others <- mycolors <- colorRampPalette(brewer.pal(9, "Blues"))(length(partners_others)) %>%
    `names<-`(partners_others)

  colors <- c(colors_russia_transit, colors_others)
  colors[["Other"]] <- "#CACACA"

  d_plot <- d %>%
    mutate(partner=tidyr::replace_na(partner, "Other"),
           partner=factor(partner, levels=names(colors)),
           country=factor(country, levels=top_n_countries)) %>%
    group_by(country, commodity, unit, source, year) %>%
    mutate(value=value/sum(value)*100) %>%
    mutate(commodity = recode(commodity, !!!commodities_rev)) %>%
    mutate(commodity_unit = sprintf("%s (%s)", commodity, unit)) %>%
    ungroup() %>%
    # Add label
    mutate(label=ifelse(value>10 & (partner %in% partners_russia_transit), sprintf("%s %.0f %%", partner, value), ""))

  d_plot <- d_plot %>%
    right_join( d_plot %>% tidyr::expand(country, commodity, year, unit, commodity_unit)) %>%
    mutate(value=tidyr::replace_na(value, 0),
           partner=tidyr::replace_na(partner, "Other"),
           label=tidyr::replace_na(label,""))


  plots <- lapply(split(d_plot, d_plot$country), function(d_country){
    d_country %>%
      group_by(partner) %>%
      arrange(partner) %>%
      plot_ly(
        x = ~value,
        y = ~commodity_unit,
        text=~label,
        color= ~partner,
        colors=colors,
        # colors = 'Reds',
        type = 'bar',
        legendgroup=~partner,
        showlegend=F) %>%
     add_annotations(text = ~label,
                          x = ~value,
                          y = ~commodity_unit,
                     xanchor="right",
                          # xref = "x",
                          # yref = "y",

                          font = list(family = 'Arial',
                                      size = 12,
                                      color="#FFFFFF"),
                          showarrow = FALSE) %>%
    plotly::layout(yaxis =
                     list(title = unique(d_country$country)),
             xaxis= list(title="%"))
  })

  plt <- do.call(function(...){subplot(..., nrows=10, titleY=T, shareX=T)}, plots) %>% layout(barmode = 'stack', showlegend = TRUE)
  plt
  return(plt)
})
