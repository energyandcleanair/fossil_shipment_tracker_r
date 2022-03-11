# Anything that calls autoInvalidate will automatically invalidate
# every 2 seconds.
autoInvalidate <- reactiveTimer(sec_per_cycle*1000)


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




# output$buttonClip <- renderUI({
#   rclipButton("clipbtn", " Copy URL", input$.shinyURL, icon("copy"))
# })
#
#
# # Output Elements --------------------------------------
output$counter_label_total <- renderUI({
  req(counter_real_time())
  total <- sum(unlist(counter_real_time()))
  HTML(scales::comma(total, accuracy=NULL, suffix=""))
})


output$counter_label_natural_gas <- renderUI({
  req(counter_real_time())
  HTML(scales::comma(counter_real_time()$natural_gas / 1e6, accuracy=NULL, suffix="M"))
})

output$counter_label_oil <- renderUI({
  req(counter_real_time())
  HTML(scales::comma(counter_real_time()$oil / 1e6, accuracy=NULL, suffix="M"))
})

output$counter_label_coal <- renderUI({
  req(counter_real_time())
  HTML(scales::comma(counter_real_time()$coal / 1e6, accuracy=NULL, suffix="M"))
})

output$counter_loader <- renderUI({
  req(counter_data())
  return(NULL)
  #remove itself
  removeUI(selector="#counter_loader")
})
#
# output$counter <- renderUI({
#
#   req(counter_real_time())
#
#   data <- counter_real_time()
#   print(data)
#   data$total <- sum(unlist(data))
#   # labels <- lapply(data, scales::label_number_si(accuracy=0.1))
#   labels <- lapply(data, function(x) scales::comma(x/1E6, accuracy=NULL, suffix="M"))
#   # names(labels) <- paste0("#", names(labels))
#
#   x <- gsubfn::gsubfn("\\w+", labels, '
#   <div class="container">
#
#     <h1>Payments to Russia for fossil fuels</h1>
#     <div class="subtitle">By European Union since 24 February 2022</div>
#     <div class="row">
#       <div class="col-xs-7">
#         <div class="big-box">
#           <span class="currency">EUR</span>total
#         </div>
#       </div>
#       <div class="col-xs-5">
#          <div class="small-box">
#           <div class="title">Oil</div>
#           <span class="currency">EUR</span>oil
#         </div>
#
#         <div class="small-box">
#           <div class="title">Fossil Gas</div>
#           <span class="currency">EUR</span>natural_gas
#         </div>
#
#         <div class="small-box">
#           <div class="title">Coal</div>
#           <span class="currency">EUR</span>coal
#         </div>
#       </div>
#   </div>')
#
#   HTML(x)
# })


# # Reactive Elements --------------------------------------
counter_flows <- reactive({
  # Don't load flows if this is not the tab queried
  # query <- parseQueryString(session$clientData$url_search)
  # if(!is.null(query$tab) && (query$tab != "counter")){
  #   return(NULL)
  # }
  db.download_flows(source="combined_light")
})


counter_add_each_sec <- reactive({
  req(counter_flows())
  p <- counter_flows() %>%
    filter(date <= lubridate::today()) %>%
    group_by(commodity, transport, source) %>%
    # Take average over last few days in case last day data is incorrect
    arrange(desc(date)) %>%
    slice_head(n=3) %>%
    mutate(commodity=recode(commodity, !!!list("crude_oil"="oil",
                                               "oil_products"="oil"))) %>%
    # filter(unit=="tonne") %>%
    group_by(date, commodity, transport) %>%
    summarise(value_eur=sum(value_eur)) %>%
    group_by(date, commodity) %>%
    summarise(value_eur=sum(value_eur)) %>%
    group_by(commodity) %>%
    summarise(value_eur=mean(value_eur)) %>%
    mutate(value_eur=value_eur/24/3600)

  as.list(p$value_eur) %>% `names<-`(p$commodity)

})


counter_data <- reactive({
  req(counter_flows())

  p <- counter_flows() %>%
    filter(date >= date_from_counter,
           date <= lubridate::today()) %>%
    mutate(commodity=recode(commodity, !!!list("crude_oil"="oil",
                                               "oil_products"="oil"))) %>%
    group_by(commodity) %>%
    summarise(value_eur=sum(value_eur, na.rm=T)) %>%
    ungroup()

  as.list(p$value_eur) %>% `names<-`(p$commodity)
})


counter_real_time <- reactive({
  req(counter_add_each_sec())
  req(counter_data())

  n_sec <- difftime(lubridate::now(), lubridate::floor_date(lubridate::now(),'day')) %>% as.numeric(units="hours") *3600
  autoInvalidate()

  lapply(names(counter_data()), function(f){
    counter_data()[[f]] + counter_add_each_sec()[[f]] * n_sec
  }) %>% `names<-`(names(counter_data()))
})





# power_raw <- reactive({
#
#   # To trigger refresh
#   # input$power_refresh
#   country <- input$country
#   year_from <- input$year_from
#   year_to <- input$year_to
#   req(country, year_from, year_to)
#
#   # Get data
#   print("Getting power data")
#   power <- creapower::get_generation(
#     date_from=sprintf("%s-01-01", year_from),
#     date_to=sprintf("%s-12-31", year_to),
#     iso2 = country,
#     homogenise = T,
#     freq = "day"
#   )
#   print("Done")
#   return(power)
# })
#
#
# power <- reactive({
#
#   power_raw <- power_raw()
#   frequency <- input$frequency
#   sources <- input$sources
#   req(power_raw, frequency, sources)
#
#   print("Processing power data")
#   power <- power_raw %>%
#     filter(source %in% sources) %>%
#     mutate(date=lubridate::floor_date(date, unit=frequency)) %>%
#     group_by(across(c(-output_mw))) %>%
#     summarise_at("output_mw", mean) %>%
#     ungroup()
#   print("Done")
#
#   return(power)
#
# })
#
#
# caption <- reactive({
#   power <- power_raw()
#   req(power)
#
#   ds <- unique(power$data_source)
#   ref <- paste0("Source: ", data_source_reference(ds),". ")
#   update <- paste0("Last updated on ", strftime(max(lubridate::date(power$date) + lubridate::hours(power$duration_hours), na.rm=T), "%d %B %Y."))
#   return(paste0(ref, update))
# })
#
#
# output$power_plot <- renderPlotly({
#
#   plot_type <- input$plot_type
#   sources <- input$sources
#   power <- power()
#   caption <- caption()
#   frequency <- isolate(input$frequency)
#
#   req(power, plot_type, sources, caption, frequency)
#
#   power_sources <- power %>% filter(source %in% sources) %>%
#     group_by(date, data_source, iso2, region) %>%
#     mutate(output_pct = output_mw / sum(output_mw)) %>%
#     ungroup()
#
#   if(plot_type=="lines"){
#     plt <- plot_ly(power_sources,
#             x = ~date,
#             y = ~output_mw,
#             color = ~source,
#             customdata=~source,
#             colors=creapower::palette_power(),
#             type = "scatter",
#             mode="lines+marker",
#             hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
#             showlegend = T) %>%
#       layout(
#         hovermode = "x unified",
#         yaxis = list(title = 'Power generation (MW)'),
#         xaxis = list(title = ''))
#   }
#
#   if(plot_type=="area"){
#     plt <- plot_ly(power_sources,
#             x = ~date,
#             y = ~output_mw,
#             color = ~source,
#             customdata = ~source,
#             colors = creapower::palette_power(),
#             type = 'scatter',
#             mode = 'lines',
#             line = list(width = 0),
#             alpha = 0.9,
#             stackgroup = 'one',
#             hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
#             showlegend = T) %>%
#       layout(
#         hovermode = "x unified",
#         yaxis = list(title = 'Power generation (MW)'),
#         xaxis = list(title = ''))
#   }
#
#   if(plot_type=="area_pct"){
#     plt <- plot_ly(power_sources,
#             x = ~date,
#             y = ~output_pct,
#             color = ~factor(source),
#             customdata = ~source,
#             colors = creapower::palette_power(),
#             type = 'scatter',
#             mode = 'lines',
#             line = list(width = 0),
#             alpha = 0.9,
#             stackgroup = 'one',
#             hovertemplate = '%{customdata} %{y:.0%}<extra></extra>',
#             showlegend = T) %>%
#       layout(
#         hovermode = "x unified",
#         yaxis = list(title = 'Share of power generation',
#                      tickformat = '.0%'),
#         xaxis = list(title = ''))
#   }
#
#   if(plot_type=="bar"){
#
#     power_deyeared <- power_sources %>%
#       mutate(year=lubridate::year(date),
#              date2000 = lubridate::`year<-`(date, 2000)) %>%
#       group_by(iso2, region, date2000, year) %>%
#       summarise(output_mw=sum(output_mw)) %>%
#       ungroup()
#
#     tickformat <- recode(frequency,
#                          "day"="%e %b",
#                          "week"= "%W",
#                          "month"="%b",
#                          "year"="%Y")
#
#     dtick <- ifelse(frequency=="month", "M1", NA)
#
#     plt <- plot_ly(power_deyeared,
#                    x = ~date2000,
#                    y = ~output_mw,
#                    color = ~factor(year),
#                    customdata = ~year,
#                    colors = 'Reds',
#                    type = 'bar',
#                    alpha = 0.9,
#                    hovertemplate = '%{customdata} %{y:,.0f} MW<extra></extra>',
#                    showlegend = T) %>%
#              layout(
#                hovermode = "x unified",
#                yaxis = list(title = 'Power generation (MW)'),
#                xaxis = list(title = '',
#                             dtick = dtick,
#                             tickformat=tickformat))
#   }
#
#   plt <- plt %>%
#     layout(
#       annotations = list(x = 1, y = 0, text = caption,
#          showarrow = F, xref='paper', yref='paper',
#          xanchor='right', yanchor='auto', xshift=0, yshift=-60,
#          font=list(color="#AAAAAA")),
#       margin = list(b=60),
#       yaxis = list(fixedrange=T))
#
#   return(plt)
# })
