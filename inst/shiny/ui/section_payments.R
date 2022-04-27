div(class="container container-payments",
  # h1("Daily payments to Russia"),
  div(class="subtitle", "Pipeline and shipments in million EUR per day"),
  plotlyOutput("plot_payments", height='600px') %>% withSpinner(color="#8cc9D0")
)
