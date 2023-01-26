agsi.get_storage_change <- function(date_from, date_to, iso2){
  lapply(iso2, function(iso2){
    url <- sprintf("https://agsi.gie.eu/api?country=%s&from=%s&to=%s&page=1&size=100000",
                   iso2, date_from, date_to)

    data <- jsonlite::read_json(url)
    data <- data$data
    data <- lapply(data, function(x){x$info=NULL; as.data.frame(x)}) %>%
      do.call(bind_rows, .) %>%
      tibble()

    data %>%
      select(iso2=code,
             date=gasDayStart,
             value_gwh=netWithdrawal) %>%
      mutate(date=date(date),
             value_gwh=as.numeric(value_gwh),
             value_m3=value_gwh*1e6/gcv_kWh_per_m3,
             type='storage_drawdown')
  }) %>%
    bind_rows()
}
