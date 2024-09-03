china.get_google_sheets_url <- function() {
  "https://docs.google.com/spreadsheets/d/e/2PACX-1vQunCwQmOpGXSLWiToq6zZDLi3VFqknU2fyDrRCtURFCT2QS1oer4H9i_eCXnyZfw/pub?output=csv"
}

china.get_flows <- function() {
  # China pipeline oil imports 41 Mtpa

  bind_rows(
    china.get_flows_natural_gas(),
    china.get_flows_pipeline_oil()
  )
}

china.get_flows_natural_gas <- function(diagnostics_folder = "diagnostics",
                                        fill_gaps_and_future = T) {
  log_info("Get flows natural gas China")

  # China pipeline gas 3.9 bcm in 2020; 5 bcm over 161 days in 2021-22 - assume 10 bcmpa
  # http://www.xinhuanet.com/english/2021-08/10/c_1310119621.htm
  # https://news.cgtn.com/news/2022-01-18/China-Russia-pipeline-delivers-15b-cubic-meters-of-natural-gas--16V6fC0jWQo/index.html

  log_info("Building base natural gas assumptions")
  original <- tibble(
    commodity = "natural_gas",
    value_m3 = c(10e9 / 365),
    value_tonne = c(10e9 / 1000 / 365 * 0.7168),
  ) %>%
    tidyr::crossing(tibble(date = seq.Date(as.Date("2021-01-01"), lubridate::today(), by = "day"))) %>%
    mutate(
      departure_iso2 = "RU",
      destination_iso2 = "CN"
    )

  log_info("Fetching China pipeline gas data")
  # Read customs data -------------------------------------------------------
  path <- china.get_google_sheets_url()
  n_lines_to_skip <- 2

  log_level(REQUEST, "Fetch customs data for China from spreadsheet")
  customs <- read_csv(path, skip = n_lines_to_skip) %>%
    select(
      date = Name,
      value_kg = `China: Volume of Imports: Pipeline Carried Natural Gas (Gaseous State): Russian Federation`,
      value_usd = `China: Value of Imports: Pipeline Carried Natural Gas (Gaseous State): Russian Federation`
    ) %>%
    filter(
      grepl("^20", date),
      value_usd > 0
    ) %>%
    mutate(
      date = floor_date(lubridate::parse_date_time(gsub("/", "-", date), orders = c("ymd", "ym")), "month"),
      value_kg = as.numeric(gsub(",", "", value_kg)),
      value_usd = as.numeric(gsub(",", "", value_usd)),
      price_usd_per_kg = case_when(
        value_usd > 0 & value_kg > 0 ~ value_usd / value_kg,
        T ~ NA
      )
    )


  # Collect prices ----------------------------------------------------------

  # https://carnegieendowment.org/politika/89552
  # It is also obvious that the pricing for all the Chinese pipeline gas contracts is in parallel,
  # most likely as a result of using the so-called 6-3-3 Brent average,
  # meaning that the price is fixed for three months based on a six-month average with a three-month delay
  # : i.e., the gas price for October–December is determined by averaging the oil price in January–June.

  log_info("Fetching monthly price indicators")
  prices <- get_prices_monthly()

  # Add a six month rolling average of Brent

  prices <- prices %>%
    arrange(date) %>%
    mutate(brent_6m = zoo::rollmean(brent, 6, align = "right", fill = NA)) %>%
    mutate(brent_6m_lag3 = lag(brent_6m, 3))

  log_info("Fetching exchange rates")
  cny_per_usd <- price.cny_per_usd(monthly = T)
  eur_per_usd <- price.eur_per_usd(monthly = T)

  log_info("Build data for model for China pipeline gas prices")
  data <- customs %>%
    # Fill dates so that we can have longer price time series
    tidyr::complete(date = seq.Date(min(date(date)), ceiling_date(today(), "month"), by = "month")) %>%
    left_join(prices, by = join_by(date)) %>%
    left_join(cny_per_usd, by = join_by(date)) %>%
    left_join(eur_per_usd, by = join_by(date)) %>%
    mutate(
      ttf_cny = ttf * cny_per_usd,
      jkm_cny = jkm * cny_per_usd,
      brent_cny = brent * cny_per_usd,
      brent_6m_lag3_cny = brent_6m_lag3 * cny_per_usd,
      price_cny_per_kg = price_usd_per_kg * cny_per_usd,
      value_cny = value_usd * cny_per_usd,
      value_eur = value_usd * eur_per_usd
    )


  # CNY version
  # model <- lm(price_cny_per_kg ~ cny_per_usd +  lag(ttf_cny, 3) + brent_cny + lag(brent_cny, 3) + brent_6m_lag3_cny
  #             , data %>% filter(value_kg > 0))
  # summary(model)
  #
  # USD version

  log_info("Build model for China pipeline gas prices")
  model <- lm(
    price_usd_per_kg ~ brent_6m_lag3 + lag(cny_per_usd, 1),
    data %>% filter(value_kg > 0)
  )
  log_info("Checking model")
  r2 <- summary(model)$r.squared
  if (r2 < 0.8) stop("Something wrong with China gas pricing model")

  log_info("Predicting China pipeline gas prices in USD")
  data$price_usd_per_kg_predicted <- predict(model, data)

  log_info("Calculating volumes")
  data <- data %>%
    mutate(
      value_kg_predicted = value_usd / price_usd_per_kg_predicted,
      value_tonne = value_kg_predicted / 1000,
      value_m3 = value_tonne * 1000 / 0.7168,
      eur_per_tonne = price_usd_per_kg_predicted * eur_per_usd * 1000
    )

  # Diagnostic plots
  if (!is.null(diagnostics_folder)) {
    ggplot(data) +
      geom_line(
        data = function(x) x %>% filter(value_kg > 0),
        aes(date, price_usd_per_kg, col = "declared")
      ) +
      geom_line(aes(date, price_usd_per_kg_predicted, col = "price model"), linetype = "dashed") +
      rcrea::scale_y_crea_zero() +
      rcrea::theme_crea() +
      labs(
        title = "Pipeline gas price from Russia to China",
        subtitle = "USD per kg",
        x = NULL,
        y = NULL, col = NULL
      )
    ggsave(file.path(diagnostics_folder, "china_pipeline_gas_price.jpg"), width = 8, height = 4)
    # ggplot(data) + geom_point(aes(price_usd_per_kg, price_usd_per_kg_predicted)) + geom_abline()
    # ggplot(data) + geom_line(aes(date, price_usd_per_kg, col='observed'))+ geom_line(aes(date, price_usd_per_kg_predicted, col='predicted')) +
    #   rcrea::scale_y_crea_zero()
    ggplot(data) +
      geom_line(
        data = function(x) x %>% filter(value_kg > 0),
        aes(date, value_kg / 1e9, col = "declared")
      ) +
      geom_line(aes(date, value_kg_predicted / 1e9, col = "price model"), linetype = "dashed") +
      geom_line(data = original, aes(as.POSIXct(date), value_tonne * 30.5 / 1e6, col = "old version in database")) +
      rcrea::scale_y_crea_zero() +
      rcrea::theme_crea() +
      labs(
        title = "Pipeline gas volumes from Russia to China",
        subtitle = "Million tonnes per month",
        x = NULL,
        y = NULL, col = NULL
      )
    ggsave(file.path(diagnostics_folder, "china_pipeline_gas_volume.jpg"), width = 8, height = 4)


    ggplot(data) +
      geom_line(
        data = function(x) x %>% filter(value_kg > 0),
        aes(date, value_kg / 0.7168 / 1e9 * 12, col = "declared")
      ) +
      geom_line(aes(date, value_kg_predicted / 0.7168 / 1e9 * 12, col = "price model"), linetype = "dashed") +
      # geom_line(data=original, aes(as.POSIXct(date), value_tonne*30.5/1e6, col='currently in database')) +
      rcrea::scale_y_crea_zero() +
      rcrea::theme_crea() +
      labs(
        title = "Pipeline gas volumes from Russia to China",
        subtitle = "BCM equivalent per year",
        x = NULL,
        y = NULL, col = NULL
      )
    ggsave(file.path(diagnostics_folder, "china_pipeline_gas_volume_bcm.jpg"), width = 8, height = 4)
  }

  log_info("Converting to daily model")
  # Make it daily
  sum_before <- sum(data$value_tonne, na.rm = T)
  result <- data %>%
    select(month = date, value_tonne, value_m3, eur_per_tonne) %>%
    left_join(
      tibble(
        date = seq.Date(min(lubridate::date(data$date)),
          lubridate::ceiling_date(max(lubridate::date(data$date)), "month") - days(1),
          by = "day"
        ),
        month = floor_date(date, "month"),
        days_in_month = lubridate::days_in_month(month),
      ),
      multiple = "all",
      by = join_by(month)
    ) %>%
    # Divide all columns starting with value_ by days_in_month
    mutate_at(vars(starts_with("value_")), ~ . / days_in_month) %>%
    mutate(
      commodity = "natural_gas",
      departure_iso2 = "RU",
      destination_iso2 = "CN"
    ) %>%
    select(-c(month, days_in_month))

  log_info("Checking that the total volumes before and after are the same")
  # Never too safe
  sum_after <- sum(result$value_tonne, na.rm = T)
  if (round(sum_before) != round(sum_after)) stop("Wrong join")
  # if(any(is.na(result$eur_per_tonne))) stop("Missing price")

  if (fill_gaps_and_future) {
    log_info("Filling gaps and future")
    result <- result %>%
      fill_gaps_and_future()
  }

  return(result)
}

china.get_natural_gas_prices <- function(fill_gaps_and_future = T) {
  flows <- china.get_flows_natural_gas(
    fill_gaps_and_future = fill_gaps_and_future,
    diagnostics_folder = NULL
  )

  flows %>%
    select(date, eur_per_tonne, departure_iso2, destination_iso2)
}

china.get_flows_pipeline_oil_old <- function() {
  # China is set to receive approximately 880,000 bpd of Russian oil via the two East Siberia-Pacific Ocean Pipelines (ESPO) and the Kazakhstan-China pipeline under government deals
  # https://www.reuters.com/business/energy/china-extends-record-imports-russian-oil-into-june-cuts-saudi-supply-trade-2022-07-06/

  # Total - shipments -> Pipeline
  #
  # tonne_per_bl = 0.136
  # old <- tibble(
  #   value_m3=c(NA,NA,NA),
  #   value_tonne=c(41e6/365, 880e3*tonne_per_bl, 880e3*tonne_per_bl),
  #   date=c(as.Date("2022-01-01"), as.Date("2022-06-01"), lubridate::today())
  # ) %>%
  #   full_join(tibble(date=seq.Date(as.Date("2022-01-01"), lubridate::today(), by="day"),
  #                    partner='China',
  #                    commodity=c('pipeline_oil'))) %>%
  #   arrange(date) %>%
  #   fill(value_tonne) %>%
  #   mutate(departure_iso2='RU',
  #          destination_iso2='CN')
  # )
}


china.get_flows_pipeline_oil <- function(use_google_sheets = T) {
  # Getting total monthly data from China customs: http://43.248.49.97/indexEn
  # Or from wind
  # And deducing shipments

  # Capping: "The estimated ESPO oil pipeline capacity for deliveries to mainland China for Russian oil is approximately 35 MMt/y"
  # max_value_tonne = 35e6/365

  api_key <- Sys.getenv("RUSSIA_FOSSIL_TRACKER_API_KEY")

  shipments <- read_csv(
    glue(
      "https://api.russiafossiltracker.com",
      "/v1/kpler_trade",
      "?date_from=2022-01-01",
      "&commodity_equivalent=crude_oil",
      "&aggregate_by=destination_month,commodity_equivalent",
      "&commodity_origin_iso2=RU",
      "&destination_iso2=CN",
      "&format=csv",
      "&api_key={api_key}"
    )
  ) %>%
    select(month, value_shipment_tonne_month = value_tonne)

  if (use_google_sheets) {
    # China Wind url:
    path <- china.get_google_sheets_url()
    skip <- 2
  } else {
    path <- system.file("extdata", "china/china_imports_wind.csv", package = "russiacounter")
    skip <- 1
  }

  total <- read_csv(path, skip = skip) %>%
    select(
      date = Name,
      value_kg = `China: Volume of Imports: Crude Oil: Russian Federation`
    ) %>%
    filter(grepl("^20", date)) %>%
    mutate(
      date = lubridate::parse_date_time(gsub("/", "-", date), orders = c("ymd", "ym")),
      month = floor_date(date, "month"),
      value_total_tonne_month = as.numeric(gsub(",", "", value_kg)) / 1000
    ) %>%
    select(month, value_total_tonne_month)

  pipeline <- shipments %>%
    left_join(total) %>%
    filter(!is.na(value_total_tonne_month)) %>%
    full_join(
      tibble(date = seq.Date(as.Date("2022-01-01"), lubridate::today() + lubridate::days(31), by = "day")) %>%
        mutate(
          month = lubridate::floor_date(date, "month"),
          ndays = lubridate::days_in_month(month)
        )
    ) %>%
    mutate(value_tonne = (value_total_tonne_month - value_shipment_tonne_month) / ndays) %>%
    # Fill missing days
    arrange(date) %>%
    tidyr::fill(value_tonne) %>%
    select(date, value_tonne) %>%
    mutate(
      departure_iso2 = "RU",
      destination_iso2 = "CN",
      commodity = "pipeline_oil",
      value_m3 = NA_real_
    ) %>%
    select(departure_iso2, destination_iso2, commodity, date, value_tonne, value_m3)

  return(pipeline)
}

china.explore <- function() {
  # So I took data from China customs here: http://43.248.49.97/indexEn
  # It seems like this site/filtering gives pipeline data directly: red is shipment, blue is our pipeline assumption, green is China customs for crude imports from Russia ('Petroleum oils&oils obtained from bituminous minerals, crude'). The numbers are those that have been mentioned by media for ESPO (e.g. the 8.42 mn tonnes here for May).
  # On China customs stats page, I can't find product x origin table... but I must miss something since all medias quoted a number... Anyway, can talk about it tomorrow. Total imports are more on the 30-40 million tonnes monthly, roughly twice our Russia -> China values, which makes sense. (edited)


  # # Check how much shipment we had and deduct
  pipeline <- read_csv("https://api.russiafossiltracker.com/v0/counter?date_from=2022-01-01&commodity=pipeline_oil&aggregate_by=month,commodity&destination_iso2=CN&format=csv")

  direct_shipment <- read_csv("https://api.russiafossiltracker.com/v0/voyage?date_from=2022-01-01&commodity=crude_oil&aggregate_by=arrival_month,commodity&destination_iso2=CN&commodity_origin_iso2=RU&format=csv") %>%
    # filter(destination_iso2!='CN') %>%
    mutate(commodity = "crude_oil directly")


  existing_kr_to_cn <- read_csv("https://api.russiafossiltracker.com/v0/voyage?date_from=2022-01-01&commodity=crude_oil&aggregate_by=arrival_month,commodity&destination_iso2=KR&commodity_destination_iso2=CN&format=csv") %>%
    # filter(destination_iso2!='CN') %>%
    mutate(commodity = "crude_oil through other country")

  all_shipments <- read_csv("https://api.russiafossiltracker.com/v0/voyage?date_from=2022-01-01&commodity=crude_oil&aggregate_by=arrival_month,commodity&commodity_origin_iso2=RU&commodity_destination_iso2=CN&format=csv") %>%
    # filter(destination_iso2!='CN') %>%
    mutate(commodity = "crude_oil")

  counter_cn <- read_csv("https://api.russiafossiltracker.com/v0/counter?date_from=2022-01-01&commodity=crude_oil,pipeline_oil&aggregate_by=month,commodity&destination_iso2=CN&format=csv")


  ggplot(bind_rows(
    counter_cn %>%
      select(month, commodity, value_tonne) %>%
      mutate(source = "Counter"),
    pipeline %>%
      select(month, commodity, value_tonne) %>%
      mutate(source = "CREA"),
    all_shipments %>%
      select(month = arrival_month, commodity, value_tonne) %>%
      mutate(source = "CREA"),
    # direct_shipment %>%
    #   select(month=arrival_month, commodity, value_tonne) %>%
    #   mutate(source='CREA'),
    # existing_kr_to_cn %>%
    #   select(month=arrival_month, commodity, value_tonne) %>%
    #   mutate(source='CREA')
  ) %>%
    filter(month <= "2022-07-01")) +
    geom_area(aes(month, value_tonne, fill = commodity)) +
    facet_wrap(~source)
}
