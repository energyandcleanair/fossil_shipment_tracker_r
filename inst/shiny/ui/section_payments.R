div(class="container container-payments",
  # h1("Daily payments to Russia"),
  div(class="subtitle", "Deliveries of Russian fossil fuel by pipeline and ship in million EUR per day"),
  plotlyOutput("plot_payments", height='700px') %>% withSpinner(color="#8cc9D0")
)
