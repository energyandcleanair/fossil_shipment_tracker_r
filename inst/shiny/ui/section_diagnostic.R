div(class="container container-diagnostic",
  h1("Diagnostics"),
  div(class="subtitle", "Pipelined gas to EU28"),
  plotlyOutput("plot_pipelined_gas_yearly", height='400px') %>% withSpinner(color="#8cc9D0"),

  div(class="subtitle", "Pipelined gas to EU28 countries"),
  div(class="", "7-day running average"),
  plotlyOutput("plot_pipeline_from_russia_ts", height='400px') %>% withSpinner(color="#8cc9D0"),



  div(class="subtitle", "Fact-checks with news"),
  div(class="",
      span(class="", '"Moscow earned $20 billion each month in 2022, selling 8 million barrels a day, 43% of this to the EU" ('),
      a(class="", href='https://www.bloomberg.com/news/articles/2022-05-12/russia-oil-revenue-up-50-this-year-despite-boycott-iea-says',
        target='_blank', "link"),
      span(class="", ")")),
  plotlyOutput("plot_monthly_oil", height='400px') %>% withSpinner(color="#8cc9D0"),


  div(class="subtitle", "Detailed payments"),
  div(class="", "7-day running average"),
  plotlyOutput("plot_payments_detailed", height='900px') %>% withSpinner(color="#8cc9D0"),


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
  plotlyOutput("plot_prices", height='600px') %>% withSpinner(color="#8cc9D0"),


  div(class="subtitle", "Counter last vs Counter"),
  div(class="", "Comparison of the two counter endpoints: one counter per day and the estimation as of now."),
  plotlyOutput("plot_counter_comparison", height='600px') %>% withSpinner(color="#8cc9D0")
)
