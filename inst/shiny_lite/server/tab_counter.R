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
output$counter_label_total <- renderUI({
  req(counter_real_time())
  HTML(scales::comma(counter_real_time()$total, accuracy=NULL, suffix=""))
})


output$counter_label_natural_gas <- renderUI({
  req(counter_real_time())
  HTML(scales::comma(counter_real_time()$gas / 1e6, accuracy=NULL, suffix="M"))
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


# # Reactive Elements --------------------------------------

counter_data <- reactive({
  db.download_counter() %>%
    filter(date==max(date))
})


counter_real_time <- reactive({
  req(counter_data())
  c <- counter_data()
  n_days <- difftime(lubridate::now(tzone = "UTC"), as.Date(c$date, tz="UTC")) %>% as.numeric(units="days")
  autoInvalidate()

  list(
    coal=c$cumulated_coal_eur + n_days * c$coal_eur,
    gas=c$cumulated_gas_eur + n_days * c$gas_eur,
    oil=c$cumulated_oil_eur + n_days * c$oil_eur,
    total=c$cumulated_total_eur + n_days * c$total_eur
  )
})
