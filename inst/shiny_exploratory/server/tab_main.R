


# Event Observers --------------------------------------

# Preset -> other select inputs
observe({

  counter <- counter_raw()
  req(counter)

  commodities <- unique(counter$commodity)
  updatePickerInput(session, "commodities",
              choices=rev(commodities),
              selected=commodities)

#   updateSelectInput(session, "sources",
#                     selected=params[["sources"]])
#
#   updateSelectInput(session, "plot_type",
#                     selected=params[["plot_type"]])
})

#
# # Other select inputs -> preset
# observe({
#   frequency <- input$frequency
#   sources <- input$sources
#   plot_type <- input$plot_type
#
#   preset <- isolate(input$preset)
#   req(frequency, preset, sources, plot_type)
#
#   params <- preset_params[[preset]]
#
#   if(
#     preset != "custom" &&
#     (
#       frequency != params[["frequency"]] ||
#       sort(sources) != sort(params[["sources"]]) ||
#       plot_type != params[["plot_type"]]
#     )
#   ){
#     updateSelectInput(session, "preset",
#                       selected="custom")
#   }
# })
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
output$downloadCsv <- downloadHandler(
  filename = function() {
    paste("data.csv", sep = "")
  },
  content = function(file) {
    write.csv(data(), file, row.names = FALSE)
  }
)


output$buttonClip <- renderUI({
  rclipButton("clipbtn", " Copy URL", input$.shinyURL, icon("copy"))
})


# Output Elements --------------------------------------
output$selectUnit <- renderUI({
  selectInput("unit", NULL,
              multiple=F,
              choices=units,
              selected=units[1])
})

output$selectPreset <- renderUI({
  selectInput("preset", NULL,
              multiple=F,
              choices=presets,
              selected="custom")
})


output$selectColourBy <- renderUI({
  selectInput("colour_by", "Colour by",
              multiple=F,
              choices=colour_bys,
              selected="commodity")
})


output$selectGroupBy <- renderUI({
  selectInput("group_by", "Group by",
              multiple=F,
              choices=group_bys,
              selected="destination_region")
})


output$selectCommodities <- renderUI({
  pickerInput("commodities", "Commodities",
              choices=c(),
              options =list(`actions-box` = TRUE),
              multiple = T,
              selected = c())
})

output$selectPlotType <- renderUI({
  selectInput("plot_type", "Plot Type", multiple=F, choices = plot_types, selected="area")
})

# output$selectYearFrom <- renderUI({
#   selectInput("year_from", "From", multiple=F,
#               choices = seq(2016, lubridate::year(lubridate::today())), selected="2018")
# })
#
# output$selectYearTo <- renderUI({
#   selectInput("year_to", "To", multiple=F,
#               choices = seq(2016, lubridate::year(lubridate::today())), selected="2021")
# })
# output$selectYears <- renderUI({
#   sliderTextInput("years", "Years",
#     choices = seq(2016, lubridate::year(lubridate::today())),
#     selected = c(2020, 2021)
#   )
# })


# Reactive Elements --------------------------------------

counter_raw <- reactive({
  read_csv("https://api.russiafossiltracker.com/v0/counter?format=csv&date_from=2021-10-01&use_eu=True")
})

commodities_raw <- reactive({
  read_csv("https://api.russiafossiltracker.com/v0/commodity?format=csv")
})

data <- reactive({
  counter_raw <- counter_raw()
  colour_by <- input$colour_by
  group_by <- input$group_by
  commodities <- input$commodities
  rolling_days <- input$rolling_days
  n_groups <- input$n_groups
  commodities_raw <- commodities_raw()
  date_from <- input$date_from
  date_to <- input$.date_to
  unit <- input$unit

  req(counter_raw, colour_by, group_by, commodities, rolling_days, commodities_raw)

  data <- counter_raw %>%
    filter(commodity %in% commodities) %>%
    filter(date>=date_from, date_to<=date_to) %>%
    left_join(commodities_raw %>% select(commodity=id, commodity_name=name)) %>%
    mutate(commodiy=commodity_name) %>%
    mutate(group = !!sym(group_by),
           colour = !!sym(colour_by))

  data <- data %>%
    group_by(colour, group, date) %>%
    summarise(value_eur=sum(value_eur, na.rm=T),
              value_tonne=sum(value_tonne, na.rm=T)) %>%
    tidyr::complete(colour, group, date,
                    fill=list(value_eur=0, vaalue_tonne=0)) %>%
    rcrea::utils.running_average(rolling_days, vars_to_avg = names(.)[grepl("value_", names(.))], min_values = rolling_days)


  data["value"] <- data[paste0("value_",unit)]

  if(unit=='tonne'){
    value_scale <- 1e3
    unit_label <- 'ktonne'
  }
  if(unit=='eur'){
    value_scale <- 1e6
    unit_label <- 'mnEUR'
  }

  # Have top importers on top
  top_groups <- data %>%
    group_by(group) %>%
    summarise(total=sum(value, na.rm=T)) %>%
    arrange(desc(total)) %>%
    mutate(i_group=row_number()) %>%
    mutate(group_w_total=factor(sprintf("%s (%s %s)", group, scales::comma(round(total/value_scale)), unit_label))) %>%
    mutate(group_w_total=factor(group_w_total, levels=.$group_w_total))

  top_colours <- data %>%
    group_by(colour) %>%
    summarise(across(starts_with("value"), ~sum(., na.rm=T))) %>%
    arrange(desc(value)) %>%
    pull(colour)

  data["colour"] <- factor(data[["colour"]], levels=rev(top_colours))
  data["group"] <- factor(data[["group"]], levels=top_groups$group)

  data <- data %>%
    left_join(top_groups) %>%
    mutate(group=factor(group, levels=top_groups$group))

  # Add label
  data <- data %>%
    mutate(label=sprintf("%s: %s %s", colour, round(value/value_scale), unit_label)) %>%
    filter(i_group<=n_groups)


  return(data)
})


output$plot_main <- renderPlotly({

  plot_type <- input$plot_type
  data <- data()
  commodities_raw <- commodities_raw()
  commodities <- input$commodities
  n_groups <- input$n_groups
  unit <- input$unit
  req(data, plot_type, commodities_raw)


  if(unit=='tonne'){
    value_scale <- 1e3
    unit_label <- 'ktonne'
  }
  if(unit=='eur'){
    value_scale <- 1e6
    unit_label <- 'mnEUR'
  }

  colourCount = length(unique(data$colour))
  # getPalette = colorRampPalette(commodity_palette)
  getPalette = colorRampPalette(brewer.pal(12, "Paired"))

  if(plot_type=="lines"){
    plt <- ggplot(data) +
      geom_line(aes(date, value/value_scale, col=colour, label=label)) +
      facet_wrap(~group_w_total) +
      scale_color_manual(values=getPalette(colourCount), name=NULL) +
      rcrea::theme_crea() +
      scale_y_continuous(limits=c(0,NA), expand=expansion(mult=c(0,0.1))) +
      labs(y=paste0(unit_label," / day"),
           x=NULL)

    plt <- ggplotly(plt, tooltip="label") %>%
      layout(
        hovermode = "x unified",
        yaxis = list(title = ''),
        xaxis = list(title = ''))
  }

  if(plot_type=="area"){

    plt <- ggplot(data) +
      geom_area(aes(date, value/value_scale, fill=colour, label=label),
               stat="identity") +
      facet_wrap(~group_w_total) +
      scale_fill_manual(values=getPalette(colourCount), name=NULL) +
      rcrea::theme_crea() +
      scale_y_continuous(limits=c(0,NA), expand=expansion(mult=c(0,0.1))) +
      labs(y=paste0(unit_label," / day"),
         x=NULL)

    plt <- ggplotly(plt, tooltip="label") %>%
      layout(
            hovermode = "x unified",
            yaxis = list(title = ''),
            xaxis = list(title = ''))

    # nplots <- length(unique(data$facet))
    # nrow <- ceiling(sqrt(nplots))
    # ncol <- ceiling(sqrt(nplots))
    #
    # plt <- data %>%
    #   group_by(facet) %>%
    #   group_map(~ plot_ly(data=.,
    #                       x = ~date,
    #                       y = ~value_eur/1e6,
    #                       color = ~colour,
    #                       legendgroup=~colour,
    #                       customdata = ~colour,
    #                       colors = getPalette(colourCount),
    #                       type = 'scatter',
    #                       mode = 'lines',
    #                       line = list(width = 0),
    #                       alpha = 0.9,
    #                       stackgroup = 'one',
    #                       hovertemplate = '%{customdata} %{y:,.0f} mnEUR<extra></extra>',
    #                       showlegend = .$destination_region[1]=="EU28"),
    #             keep=TRUE) %>%
    #   subplot(nrows = nrow, shareX = F, shareY=TRUE) %>%
    #   layout(
    #     hovermode = "x unified",
    #     yaxis = list(title = 'million EUR'),
    #     xaxis = list(title = ''))
  }

  if(plot_type=="area_pct"){

    data_plot <- data %>%
      select(date, colour, group, group_w_total, value) %>%
      mutate(value=tidyr::replace_na(value, 0)) %>%
      tidyr::complete(group, colour, date, fill=list(value=0))

    data_plot <- data_plot %>%
      left_join(data_plot %>%
                  mutate(value=tidyr::replace_na(value, 0)) %>%
                  group_by(group, date) %>%
                  summarise(total_date=sum(value, na.rm=T))) %>%
      # filter(total_date>0) %>%
      mutate(value_pct = tidyr::replace_na(value / total_date * 100,0)) %>%
      ungroup() %>%
      arrange(date)
      # filter(value_pct>0)

    plt <- ggplot(data_plot) +
      # geom_line(aes(date, value_pct, col=colour),
      #           stat="identity") +
      geom_area(aes(date, value_pct, fill=colour),
                stat="identity") +
      facet_wrap(~group_w_total) +
      scale_fill_manual(values=getPalette(colourCount), name=NULL) +
      rcrea::theme_crea() +
      scale_y_continuous(limits=c(0,101), expand=expansion(mult=c(0,0))) +
      labs(y=paste0(unit_label," / day"),
           x=NULL)

    plt <- ggplotly(plt) %>%
      layout(
        hovermode = "x unified",
        yaxis = list(title = ''),
        xaxis = list(title = ''))
  }

  if(plot_type=="bar"){

    data_plt <- data %>%
      group_by(group, colour) %>%
      summarise(value=sum(value, na.rm=T)) %>%
      mutate(label=sprintf("%s: %s %s", colour, scales::comma(round(value/value_scale)), unit_label)) %>%
      mutate(group=factor(group, levels=rev(levels(group))))

    plt <- ggplot(data_plt) +
      geom_bar(aes(value/value_scale,
                   group,
                   fill=colour,
                   label=label),
                stat="identity") +
      scale_fill_manual(values=getPalette(colourCount), name=NULL) +
      rcrea::theme_crea() +
      scale_x_continuous(limits=c(0,NA), expand=expansion(mult=c(0,0.1))) +
      labs(y=paste0(unit_label," / day"),
           x=NULL)

    plt <- ggplotly(plt, tooltip="label") %>%
      layout(
        hovermode = "x unified",
        yaxis = list(title = ''),
        xaxis = list(title = ''))
  }

  plt <- plt %>%
    layout(
      # annotations = list(x = 1, y = 0, text = caption,
      #    showarrow = F, xref='paper', yref='paper',
      #    xanchor='right', yanchor='auto', xshift=0, yshift=-60,
      #    font=list(color="#AAAAAA")),
      margin = list(b=60),
      yaxis = list(fixedrange=T))

  return(plt)
})
