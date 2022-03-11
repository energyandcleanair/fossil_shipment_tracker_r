tabPanel("Natural gas flows",
         value="flows",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             # shinyjs::useShinyjs(),

             # uiOutput("selectSource"),
             # uiOutput("selectCommodity"),
             uiOutput("selectUnit"),
             # selectInput("color_by", "Color by:", multiple=F, choices = color_bys, selected=color_bys[2]),
             # selectInput("group_by", "Group by:", multiple=F, choices = group_bys, selected=group_bys[1]),
             # selectInput("chart_type", "Chart type:", multiple=F, choices = chart_types, selected=chart_types[1]),
             # uiOutput("selectUnit"),
             downloadButton(outputId="download_flows_csv", "Download (.csv)", class="btn-secondary")
           ),
           # Show a plot of the generated distribution
           mainPanel(
             width=10,
             div(class="container_trade",
                 h1("European natural gas imports from Russia"),
                 div(class="subtitle", "Pipeline distribution only"),
                 plotlyOutput("plot_flows", height='calc(100vh - 180px)') %>% withSpinner(color="#8cc9D0"),
                 div(class="row",
                     div(class="col-xs-12 caption", span("Source: CREA analysis based on ENTSOG")
                     )
                 )
             )
           )
         )
)
