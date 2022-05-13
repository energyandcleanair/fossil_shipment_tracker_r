div(class="container container-diagnostic",
  h1("Diagnostics"),
  div(class="subtitle", "Pipelined gas to EU28"),
  plotlyOutput("plot_pipelined_gas_yearly", height='600px') %>% withSpinner(color="#8cc9D0"),

  div(class="subtitle", "2020 Net imports of pipelined gas by country"),

  plotlyOutput("plot_flows_comparison", height='600px') %>% withSpinner(color="#8cc9D0"),
  div(class="", "Red line represents country's annual consumption (source: BP)."),

  div(class="subtitle", "Commodity pricing"),
  plotlyOutput("plot_prices", height='600px') %>% withSpinner(color="#8cc9D0")



)
