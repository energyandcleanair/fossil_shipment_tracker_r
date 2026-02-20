utils.add_forecast <- function(flows) {
  predict <- function(df, keys) {
    df <- df %>%
      arrange(date) %>%
      filter(date >= max(date) - lubridate::years(3)) %>%
      tidyr::complete(
        date = seq.Date(as.Date(min(df$date)),
          as.Date(max(df$date)),
          by = "month"
        ),
        fill = list(value_tonne = 0)
      )

    dfts <- ts(df$value_tonne,
      frequency = 12,
      start = c(
        lubridate::year(min(df$date)),
        lubridate::month(min(df$date))
      )
    )

    end_date <- lubridate::today() + lubridate::days(30)
    n_months <- lubridate::interval(max(df$date), end_date) %/% months(1)
    # components_dfts <- decompose(dfts)
    # plot(components_dfts)
    tryCatch(
      {
        model <- HoltWinters(dfts)
        predicted <- forecast(model, h = n_months)

        predicted %>%
          sweep::sw_sweep(.) %>%
          filter(key == "forecast") %>%
          mutate(date = as.Date(paste("01", index), "%d %b %Y")) %>%
          select(date, value_tonne = value) %>%
          # Cap at latest observed value! Very conservative
          mutate(value_tonne = pmax(pmin(value_tonne, tail(df$value_tonne, 1)), 0))
      },
      error = function(error) {
        log_warn("Failed to forecast")
        # If failed, we just assume constant value
        return(tibble(
          date = seq.Date(max(df$date) + months(1), end_date, by = "month"),
          value_tonne = tail(df$value_tonne, 1)
        ))
      }
    )
  }

  forecasted <- flows %>%
    group_by(departure_iso2, destination_iso2, commodity) %>%
    group_modify(predict)

  bind_rows(
    flows %>% mutate(type = "observed"),
    forecasted %>% mutate(type = "forecasted")
  )
}

utils.split_month_in_days <- function(df, value_cols) {
  df %>%
    rename(month = date) %>%
    left_join(
      tibble(date = seq(min(df$date), max(df$date) + lubridate::days_in_month(max(df$date)) - 1, by = "day")) %>%
        mutate(
          weight = 1 / lubridate::days_in_month(date),
          month = lubridate::floor_date(date, "month")
        )
    ) %>%
    mutate_at(value_cols, function(x) {
      x * .$weight
    }) %>%
    arrange(desc(date)) %>%
    select(-c(weight, month))
}

utils.get_eu_iso2s <- function() {
  codelist$iso2c[!is.na(codelist$eu28) & codelist$eu28 != "NA"]
}
