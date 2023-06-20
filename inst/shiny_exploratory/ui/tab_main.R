tabPanel("Charts",
         value="power",

         sidebarLayout(
           sidebarPanel(
             width = 2,
             class ="no-padding",
             shinyjs::useShinyjs(),

             # Baseline
             # h4("Data"),
             uiOutput("selectGroupBy"),
             uiOutput("selectColourBy"),
             div(
               class="row-inline",
               height=50,
               numericInput("n_groups", "Maximum groups", 20, 1, 30, 20, 1),
               numericInput("rolling_days", "Running days", 7, 1, 31, 7, 1)
             ),
             div(
               class="row-inline",
               height=50,
               dateInput("date_from", "From", "2022-02-24", "2022-01-01", lubridate::today()),
               shinyjs::hidden(dateInput("date_to", "", NULL, "2022-01-01", lubridate::today())),
               checkboxInput("date_to_specified", "To", value=F)
             ),

             # h4("Filtering"),
             uiOutput("selectCommodities"),
             uiOutput("selectUnit"),

             # h4("Customise"),
             # uiOutput("selectSources"),
             # uiOutput("selectFrequency"),
             uiOutput("selectPlotType"),



             h4("Download"),
             downloadButton(outputId="downloadCsv",
                            "Download (.csv)",
                            class="btn-secondary"),
             h4("Share"),
             shinyURL.ui(label = NULL, copyURL=F, tinyURL=F),
             uiOutput("buttonClip")
           ),

           mainPanel(
             width=10,
             # htmlOutput("power_message", class="hia-msg"),
             plotlyOutput(".plot_main", height="calc(100vh - 90px)") %>% withSpinner(color="#8cc9D0")
           )
         )
)
