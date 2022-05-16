div(class="container container-diagnostic",
  h1("Diagnostics"),
  div(class="subtitle", "Pipelined gas to EU28"),
  plotlyOutput("plot_pipelined_gas_yearly", height='400px') %>% withSpinner(color="#8cc9D0"),

  div(class="subtitle", "2020 Net imports of pipelined gas by country"),
  div(class="", "Note: quite a few countries have NA values in IEA dataset, potentially explaining why it is lower."),
  plotlyOutput("plot_flows_comparison2020", height='900px') %>% withSpinner(color="#8cc9D0"),
  div(class="", "Red line represents country's annual consumption (source: BP)."),

  div(class="subtitle", "2021 Net imports of pipelined gas by country"),
  div(class="", "Note: quite a few countries have NA values in IEA dataset, potentially explaining why it is lower."),
  plotlyOutput("plot_flows_comparison2021", height='900px') %>% withSpinner(color="#8cc9D0"),
  div(class="", "Red line represents country's annual consumption (source: BP)."),

  div(class="subtitle", "2022 Net imports of pipelined gas by country"),
  div(class="", "Note: quite a few countries have NA values in IEA dataset, potentially explaining why it is lower."),
  plotlyOutput("plot_flows_comparison2022", height='900px') %>% withSpinner(color="#8cc9D0"),
  div(class="", "Red line represents country's annual consumption (source: BP)."),


  div(class="subtitle", "Commodity pricing"),
  plotlyOutput("plot_prices", height='600px') %>% withSpinner(color="#8cc9D0")



)
