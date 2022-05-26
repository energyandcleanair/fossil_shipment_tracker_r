div(class="container container-countries",
  # h1("Daily payments to Russia"),
  div(class="subtitle", "Largest imports of fossil fuels from Russia"),
  plotlyOutput("plot_topimporters", height='600px') %>% withSpinner(color="#8cc9D0")
)
