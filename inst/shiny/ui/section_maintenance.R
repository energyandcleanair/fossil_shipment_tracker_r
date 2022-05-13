
# shinyjs::useShinyjs(),
div(class="container container-counter",
   h1("Payments to Russia for fossil fuels"),
   div(class="subtitle", "By European Union since 24 February 2022"),
   div(class="row",
       div(class="col-md-12 col-xs-12",
           div(class="big-box",
               # span(class="currency","EUR"),
               HTML("<div style='text-align: center; margin-top: 40px; margin-bottom: 20px; color: #f6b26b;'>Under maintenance</div>")
               # htmlOutput("counter_loader") %>% withSpinner(color="#8cc9D0"),
               # htmlOutput("counter_label_total", inline=F, class="counter")
           )
       ),
   ),
   div(class="row",
       div(class="col-md-4 col-xs-12",
           div(class="small-box",
               div(class="title","Oil"),
               span(class="currency","EUR"),
               "-"
               # htmlOutput("counter_label_oil", inline=T)
           )
       ),
       div(class="col-md-4 col-xs-12",
           div(class="small-box",
               div(class="title","Fossil gas"),
               span(class="currency","EUR"),
               "-"
               # htmlOutput("counter_label_natural_gas", inline=T)
           )
       ),
       div(class="col-md-4 col-xs-12",
           div(class="small-box",
               div(class="title","Coal"),
               span(class="currency","EUR"),
               "-"
               # htmlOutput("counter_label_coal", inline=T)
           )
       )
   )
)

