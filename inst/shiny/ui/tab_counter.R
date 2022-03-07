tabPanel("Counter",
         value="counter",
          mainPanel(
               width=12,
               htmlOutput("counter") %>% withSpinner(color="#8cc9D0")
               # plotlyOutput("power_plot", height="calc(100vh - 90px)") %>% withSpinner(color="#8cc9D0")
             )
         )

