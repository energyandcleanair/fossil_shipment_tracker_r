# Predefined variables
oilprice.future_to_oilprice_id <- list(
  "urals" = "4466",
  "brent" = "46",
  "ttf" = "4447",
  "jkm" = "4448"
)

# Initialize a session

oilprice.get_handle <- function(){
  httr::handle("https://oilprice.com")
}

oilprice.get_crsf <- function(handle) {
  # Request a widget to get the CSRF token

  r <- httr::GET("https://oilprice.com/freewidgets/get_oilprices_chart/45/4",
           handle=handle)
  httr::cookies(r)[httr::cookies(r)$name=='productionop_csrf_cookie',]$value
}

# Function to get prices
oilprice.get_prices <- function(symbol) {
  if (!symbol %in% names(oilprice.future_to_oilprice_id)) {
    stop(paste("Symbol", symbol, "is not currently supported by the OilPriceDotCom client"))
  }

  handle <- oilprice.get_handle()
  crsf <- oilprice.get_crsf(handle=handle)

  headers <- c(
    "content-type" = "application/x-www-form-urlencoded; charset=UTF-8",
    "x-requested-with" = "XMLHttpRequest"
  )

  # Make the request
  (response <- httr::POST("https://oilprice.com/freewidgets/json_get_oilprices",
                   body = list('blend_id' = oilprice.future_to_oilprice_id[[symbol]],
                            'period' = '7',
                            'op_csrf_token' = crsf),
                   encode = "form",
                   httr::add_headers(.headers = headers)
  ))


  # Process response
  prices <- httr::content(response, as = "parsed", type = "application/json")$prices

  result <- lapply(prices, function(p) list(symbol = as.numeric(p$price),
                                            date = lubridate::floor_date(as.POSIXct(p$time, origin = "1970-01-01"), "day"))) %>%
    bind_rows() %>%
    rename(!!symbol:=symbol) %>%
    arrange(desc(date))


  return(result)
}



