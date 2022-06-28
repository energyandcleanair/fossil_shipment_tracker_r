


# Event Observers --------------------------------------


# Download Handlers ----------------------------------

# Downloadable csv of selected dataset
# output$downloadGeojson <- downloadHandler(
#   filename = function() {
#     paste("voyages.geojson", sep = "")
#   },
#   content = function(file) {
#     sf::st_write(voyages_sf(), file, row.names = FALSE)
#   }
# )
#
# output$downloadCsv <- downloadHandler(
#   filename = function() {
#     paste("voyages.csv", sep = "")
#   },
#   content = function(file) {
#     sf::st_write(voyages(), file, row.names = FALSE)
#   }
# )

observeEvent(input$clearSelection, {
  tableProxy <- dataTableProxy('.table_voyages')
  tableProxy %>% selectRows(NULL)

  proxy <- leafletProxy('map_voyages')
  if(!is.null(prev_lines()))
  {
    for(i in seq(nrow(prev_lines()))){
      proxy %>%
        removeShape(layerId=paste0('selected_lines_', prev_lines()$id[i]))
    }
  }

})

output$selectCommodity <- renderUI({
  commodities <- c("crude_oil","oil_or_chemical","oil_products","lng","bulk","coal")
  selectInput("commodity", NULL,
                multiple=T,
                choices=c("crude_oil","oil_or_chemical","oil_products","lng","bulk","coal"),
                selected=commodities)
  })


# Output Elements --------------------------------------


# Reactive Elements --------------------------------------

all_voyages <- reactive({
  api.get_voyages(date_from="2022-06-01")
})


voyages <- reactive({
  # req(input$commodity)
  req(all_voyages())
  all_voyages() %>%
    # filter(commodity %in% input$commodity) %>%
    mutate(quantity_unit=sprintf("%s tonne", value_tonne)) %>%
    select(id,
           status,
           departure=departure_date_utc,
           departure_port=departure_port_name,
           departure_berth=departure_berth_name,
           arrival=arrival_date_utc,
           arrival_port=arrival_port_name,
           arrival_berth=arrival_berth_name,
           arrival_iso2=arrival_iso2,
           destination_iso2=destination_iso2,
           imo=ship_imo,
           type=ship_type,
           subtype=ship_subtype,
           insurer=ship_insurer,
           owner=ship_owner,
           dwt=ship_dwt,
           commodity=commodity,
           quantity_unit=quantity_unit,
           value_tonne,
           value_m3)
})

all_voyages_sf <- reactive({
  api.get_voyages_sf(date_from="2022-04-01")
})

voyages_sf <- reactive({
  # req(input$commodity)
  req(all_voyages_sf())
  all_voyages_sf() %>%
    # filter(commodity %in% input$commodity) %>%
    mutate(quantity_unit=sprintf("%s tonne", value_tonne)) %>%
    select(id,
           status,
           departure=departure_date_utc,
           departure_port=departure_port_name,
           departure_berth=departure_berth_name,
           arrival=arrival_date_utc,
           arrival_port=arrival_port_name,
           arrival_berth=arrival_berth_name,
           arrival_iso2=arrival_iso2,
           destination_iso2=destination_iso2,
           imo=ship_imo,
           type=ship_type,
           subtype=ship_subtype,
           insurer=ship_insurer,
           owner=ship_owner,
           dwt=ship_dwt,
           commodity=commodity,
           quantity_unit=quantity_unit,
           value_tonne,
           value_m3)
})


berths_sf <- reactive({
  api.get_berths_sf()
})



output$map_voyages <- renderLeaflet({
  req(voyages_sf())
  req(berths_sf())

  commodity_groups <- c("crude_oil"="Crude oil",
                        "oil_products"="Oil products",
                        "oil_or_chemical"="Oil products",
                        "lng"="LNG",
                        "coal"="Coal",
                        "bulk_not_coal"="Other",
                        "general_cargo"="Other",
                        "lpg"="Other"
  )

  v <- voyages_sf()

  pal <- colorFactor("viridis", unique(commodity_groups))
  leaflet <- leaflet() %>%
    addTiles(group = "OpenStreetmap") %>%
    addProviderTiles('Esri.WorldImagery', group = "Satellite")

  v <- v %>%
    mutate(commodity=recode(commodity, !!!commodity_groups)) %>%
    filter(!sf::st_is_empty(geometry))

  for(v_commodity in split(v, v$commodity)){
    leaflet <- leaflet %>%
      addPolylines(data = v_commodity,
                 layerId = paste0("voyage_",v_commodity$id),
                 weight = 2,
                 color = ~pal(commodity),
                 group = unique(v_commodity$commodity))
  }

  leaflet <- leaflet %>%
    addPolygons(data=berths_sf(),
                layerId = paste0("berth_", berths_sf()$id),
                color = "green",
                group = "Berths") %>%
    addLayersControl(
      baseGroups = c("OpenStreetmap", "Satellite"),
      overlayGroups = c(unique(commodity_groups), "Berths", "Selected voyages"),
      options = layersControlOptions(collapsed = FALSE)
    )
})


output$.table_voyages <- renderDataTable({
  DT::datatable(voyages(),
                # selection = "single",
                rownames = FALSE,
                options=list(stateSave = TRUE,
                             autoWidth = TRUE,
                             scrollY = '380px'))
})


observeEvent(input$map_voyages_shape_click, {
  print(input$map_voyages_shape_click)
  id <- input$map_voyages_shape_click$id
  if(grepl("voyage_", id)){
    id <- gsub("voyage_","",id)
    dataTableProxy(".table_voyages") %>%
      selectRows(which(as.character(voyages()$id) == id)) %>%
      selectPage(which(as.character(voyages()[input$.table_voyages_rows_all,]$id) == id) %/% input$.table_voyages_state$length + 1)
  }
})


observeEvent(input$.table_voyages_rows_selected, {
  id = voyages()[input$.table_voyages_rows_selected,"id"][[1]]
  lines <- voyages_sf() %>% filter(id %in% !!id)

  proxy <- leafletProxy('map_voyages')

  # Reset previously selected marker
  if(!is.null(prev_lines()))
  {
    for(i in seq(nrow(prev_lines()))){
      proxy %>%
        # removeShape(layerId=paste0("selected_",prev_line()$id))
        removeShape(layerId=paste0('selected_lines_', prev_lines()$id[i]))
    }
  }

  if(nrow(lines)>0){
    for(i in seq(nrow(lines))){
      proxy %>%
        # addPolylines(data=line,
        #              layerId = paste0("selected_",line$id),
        #              color="red",
        #              weight=5)
        addPolylines(data=lines[i,],
                     layerId = paste0('selected_lines_', lines$id[i]),
                     color='red', #rev(RColorBrewer::brewer.pal(nrow(lines) + 5,"Reds"))[i],
                     weight=5,
                     group = "Selected voyages")
    }
  }

  # set new value to reactiveVal
  prev_lines(lines)
})




# to keep track of previously selected lines
prev_lines <- reactiveVal()
