tabPanel("Counter",
         value="counter",

          mainPanel(
               width=12,
               # shinyjs::useShinyjs(),
               div(class="container",
                   h1("Payments to Russia for fossil fuels"),
                   div(class="subtitle", "By European Union since 24 February 2022"),
                   div(class="row",
                       div(class="col-xs-8",
                           div(class="big-box",
                               span(class="currency","EUR"),
                               htmlOutput("counter_loader") %>% withSpinner(color="#35416C"),
                               htmlOutput("counter_label_total", inline=F, class="counter")
                               )
                           ),
                       div(class="col-xs-4",
                           div(class="small-box",
                               div(class="title","Oil"),
                               span(class="currency","EUR"),
                               htmlOutput("counter_label_oil", inline=T)
                           ),
                           div(class="small-box",
                               div(class="title","Fossil gas"),
                               span(class="currency","EUR"),
                               htmlOutput("counter_label_natural_gas", inline=T)
                           ),
                           div(class="small-box",
                               div(class="title","Coal"),
                               span(class="currency","EUR"),
                               htmlOutput("counter_label_coal", inline=T)
                           )
                       )
                   )
               )
               # <div class="container">
               #
               #   <h1>Payments to Russia for fossil fuels</h1>
               #   <div class="subtitle">By European Union since 24 February 2022</div>
               #   <div class="row">
               #    <div class="col-xs-7">
               #      <div class="big-box">
               #   <span class="currency">EUR</span>total
               # </div>
               #   </div>
               #   <div class="col-xs-5">
               #   <div class="small-box">
               #   <div class="title">Oil</div>
               #   <span class="currency">EUR</span>oil
               # </div>
               #
               #   <div class="small-box">
               #   <div class="title">Fossil Gas</div>
               #   <span class="currency">EUR</span>natural_gas
               # </div>
               #
               #   <div class="small-box">
               #   <div class="title">Coal</div>
               #   <span class="currency">EUR</span>coal
               # </div>
               #   </div>
               #   </div>')



               # htmlOutput("counter") %>% withSpinner(color="#8cc9D0")
               # plotlyOutput("power_plot", height="calc(100vh - 90px)") %>% withSpinner(color="#8cc9D0")
             )
         )

