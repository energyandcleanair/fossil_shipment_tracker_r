# --------------------------
# Predefined variables for the oilprice.com client
oilpricecom.future_to_oilprice_id <- list(
  "urals" = "4466",
  "brent" = "46",
  "espo" = "4183",
  "sokol" = "4410",
  "ttf" = "4447",
  "jkm" = "4448"
)

# Function to get prices
oilpricecom.get_prices_for_oil <- function(symbol) {
  if (!symbol %in% names(oilpricecom.future_to_oilprice_id)) {
    stop(paste("Symbol", symbol, "is not currently supported by the OilPriceDotCom client"))
  }

  handle <- oilpricecom.get_handle()
  crsf <- oilpricecom.get_crsf(handle = handle)

  headers <- c(
    "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
    "x-requested-with" = "XMLHttpRequest"
  )

  body <- list(
    "blend_id" = oilpricecom.future_to_oilprice_id[[symbol]],
    "period" = "7",
    "op_csrf_token" = crsf
  )

  url <- "https://oilprice.com/freewidgets/json_get_oilprices"

  format_values <- function(x) {
    paste(names(x), x, sep = ": ", collapse = " | ")
  }

  log_info(glue("Requesting prices for {symbol}: {url} - body: [{format_values(body)}] - headers: [{format_values(headers)}]"))

  log_level(REQUEST, glue("Requesting prices for {symbol}"))
  # Make the request
  (response <- httr::POST(url,
    body = body,
    encode = "form",
    httr::add_headers(.headers = headers)
  ))


  # Process response
  prices <- httr::content(response, as = "parsed", type = "application/json")$prices

  result <- lapply(prices, function(p) {
    list(
      symbol = as.numeric(p$price),
      date = lubridate::floor_date(as.POSIXct(p$time, origin = "1970-01-01"), "day")
    )
  }) %>%
    bind_rows() %>%
    rename(!!symbol := symbol) %>%
    arrange(desc(date))


  return(result)
}
attr(oilpricecom.get_prices_for_oil, "source") <- "oilprice.com"

# Initialize a session
oilpricecom.get_handle <- function() {
  httr::handle("https://oilprice.com")
}

oilpricecom.get_crsf <- function(handle) {
  # Request a widget to get the CSRF token

  log_level(REQUEST, "Requesting CSRF tokenfor oilprice.com")
  r <- httr::GET("https://oilprice.com/freewidgets/get_oilprices_chart/45/4",
    handle = handle
  )
  httr::cookies(r)[httr::cookies(r)$name == "productionop_csrf_cookie", ]$value
}


tradingeconomics.user_agent <- httr::user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3")

tradingeconomics.get_request_details <- function(commodity) {
  # Make the request
  log_level(REQUEST, glue("Getting request details from website for trading economics: {commodity}"))
  response <- httr::GET(glue("https://tradingeconomics.com/commodity/{commodity}-oil"), tradingeconomics.user_agent)

  # Check the status code
  if (response$status_code != 200) {
    stop(paste("Request failed with status code", response$status_code))
  }

  # Parse the response
  content <- httr::content(response, as = "text", type = "text/html")

  var_match_regex <- function(var_name) {
    glue("{var_name}\\s*=\\s*'([^']*)';")
  }

  extract_var <- function(var_name) {
    (content %>%
      stringr::str_match(var_match_regex(var_name)))[1, 2]
  }

  # Extract the data
  datasource_url <- extract_var("TEChartsDatasource")
  charts_token <- extract_var("TEChartsToken")
  obfusication_key <- extract_var("TEObfuscationkey")

  return(
    list(
      "datasource_url" = datasource_url,
      "charts_token" = charts_token,
      "obfusication_key" = obfusication_key
    )
  )
}

tradingeconomics.deobfusicate_prices <- function(content, obfusication_key) {
  # When tradingeconomics obfusicates the data they apply the following
  # transformations in order:
  # 1. compress using zlib deflate (equivalent to memCompress)
  # 2. for each byte, do a bitwise xor with the char at the same index of the
  #    obfusication key (where the key is repeated to match the length of the
  #    data)
  # 3. encode the result to base 64
  #
  # We apply the reverse transformations to get the original data.

  # Decode the base64
  to_decode <- as.numeric(openssl::base64_decode(content))
  # Bitwise xor with the obfusication key
  decoding_key <- utf8ToInt(obfusication_key)
  decoding_block <- rep(decoding_key, length.out = length(to_decode))
  decoded <- bitwXor(to_decode, decoding_block)
  # Decompress the
  decompressed <- memDecompress(as.raw(decoded))
  json_as_str <- rawToChar(decompressed)

  # Parse the JSON
  return(jsonlite::fromJSON(json_as_str))
}

tradingeconomics.get_prices_for_oil <- function(commodity) {
  details <- tradingeconomics.get_request_details(commodity)

  interval <- "1d"
  span <- "10y"
  ohlc <- "0"
  key <- details$obfusication_key
  charts_token <- details$charts_token
  base_url <- details$datasource_url

  url <- glue("{details$datasource_url}/markets/urdb:com")

  full_url <- httr::modify_url(url, query = list(
    "interval" = interval,
    "span" = span,
    "ohlc" = ohlc,
    "key" = charts_token
  ))

  log_level(REQUEST, "Requesting prices for {commodity} from trading economics: {full_url}")
  response <- httr::GET(full_url, tradingeconomics.user_agent)

  if (response$status_code != 200) {
    stop(paste("Request failed with status code", response$status_code))
  }

  content <- httr::content(response, as = "parsed", type = "application/json")

  log_info(glue("Deobfusicating prices for {commodity}"))
  json_response <- tradingeconomics.deobfusicate_prices(content, key)

  data_list_of_lists <- json_response$series$data[[1]]

  log_info(glue("Processing prices for {commodity} to uniform format"))
  data <- tibble::as_tibble(data_list_of_lists, .name_repair = ~ c("date", commodity, "change", "change_pc"))

  return(
    data %>%
      mutate(date = as.POSIXct(date, origin = "1970-01-01")) %>%
      arrange(desc(date)) %>%
      dplyr::select(date, !!commodity)
  )
}
attr(tradingeconomics.get_prices_for_oil, "source") <- "tradingeconomics"

oilprice.fill_gaps_and_future <- function(result, end_date) {
  fill_gaps_for_period <- function(result, end_date_sub, reason) {
    result %>%
      ungroup() %>%
      tidyr::complete(date = seq(
        lubridate::date(min(date)),
        end_date_sub,
        by = "day"
      )) %>%
      arrange(desc(date)) %>%
      rowwise() %>%
      mutate(origin = case_when(
        any(is.na(dplyr::across(-date))) ~ reason,
        .default = origin
      )) %>%
      ungroup() %>%
      tidyr::fill(setdiff(names(.), "date"), .direction = "up")
  }

  with_filled <- result %>%
    mutate(origin = "original") %>%
    fill_gaps_for_period(max(result$date), "gaps") %>%
    fill_gaps_for_period(end_date, "predicted")

  message("Filled table:")
  message(with_filled %>%
    head(5) %>%
    knitr::kable() %>%
    paste(collapse = "\n"))

  return(with_filled)
}

oilprice.check_recent_data_not_missing <- function(result, oil_type, end_date) {
  most_recent_date <- max(result$date)
  if (most_recent_date < end_date - 14) {
    stop(glue("Missing recent data for {oil_type}, most recent date was {most_recent_date}"))
  }
  return(result)
}

# We expect this function to return values in the format:
# tibble(<commodity_name>, date)
# We expect that each of these will also have an attribute with the source.
commodity_to_function <- list(
  "urals" = tradingeconomics.get_prices_for_oil,
  "brent" = oilpricecom.get_prices_for_oil,
  "espo" = oilpricecom.get_prices_for_oil,
  "sokol" = oilpricecom.get_prices_for_oil,
  "ttf" = oilpricecom.get_prices_for_oil,
  "jkm" = oilpricecom.get_prices_for_oil
)

oilprice.get_price_for_oil <- function(oil_type, end_date) {
  func <- commodity_to_function[[oil_type]]
  source <- attributes(func)$source
  log_info(glue("Getting prices for {oil_type} from {source}"))

  func(oil_type) %>%
    mutate(date = as.Date(date)) %>%
    oilprice.fill_gaps_and_future(end_date) %>%
    mutate(date = as.Date(date)) %>%
    rename(value_dpb = oil_type)
}

oilprice.get_prices_for_oils <- function(oil_types, end_date) {
  lapply(oil_types, function(oil_type) {
    oilprice.get_price_for_oil(oil_type, end_date)
  }) %>%
    bind_rows(.id = "oil_type")
}
