tabPanel("Map",
         value="shipments",

         sidebarLayout(
           sidebarPanel(
           width = 2,
           class ="no-padding",
           shinyjs::useShinyjs(),

           # Baseline
           # h4("Region"),
           # uiOutput("selectCommodity"),

           # h4("Presets"),
           # uiOutput("selectBerth"),
           div(HTML("<br>")),
           # h4("Statistics"),
           # uiOutput("statistics"),

           # downloadButton(outputId="downloadGeojson","Download (.geojson)",class="btn-secondary"),
           # downloadButton(outputId="downloadCsv","Download (.csv)",class="btn-secondary")
#
#            # h4("Customise"),
#            uiOutput("selectSources"),
#            uiOutput("selectFrequency"),
#            uiOutput("selectPlotType"),
#
#            div(
#              class="row-inline",
#              height=50,
#              uiOutput("selectYearFrom"),
#              uiOutput("selectYearTo")
#            ),


#
#            h4("Download"),
#            downloadButton(outputId="downloadCsv",
#                           "Download (.csv)",
#                           class="btn-secondary"),
#            h4("Share"),
#            shinyURL.ui(label = NULL, copyURL=F, tinyURL=F),
#            uiOutput("buttonClip")
         ),

           mainPanel(
             width=10,
             div(class="no-padding",
             leafletOutput('map_voyages', width="100%", height='calc(100vh - 550px)')
             ),
             dataTableOutput('table_voyages') %>% withSpinner(color="#8cc9D0"),
               div(
                 class="row-inline",
                 height=40,


               )
           )
    )
)
