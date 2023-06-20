div(class="container container-monthly",
  # h1("Daily payments to Russia"),
  div(class="subtitle", "Russia's estimated revenues from fossil fuel exports"),
  plotlyOutput("plot_monthly", height='700px') %>% withSpinner(color="#8cc9D0")
)
