kpler.get_flows_raw_single <- function(from=NULL,
                                product=NULL,
                                from_installation=NULL,
                                split =  "Destination Countries",
                                granularity = "daily",
                                unit = "t",
                                date_from = lubridate::today() - days(365),
                                date_to = lubridate::today()){

  library(lubridate)
  library(httr)
  library(pbapply)
  library(tidyverse)

  readRenviron(".Renviron")

  params = list(
    fromZones = from,
    products = product,
    fromInstallations=from_installation,
    flowDirection = "Export",
    split =  split,
    granularity = "daily",
    startDate = lubridate::today() - days(365),
    endDate = lubridate::today(),
    unit = unit,
    withForecast = "false",
    withIntraCountry = "false")

  params <- Filter(Negate(is.null), params)

  headers = c(
    'Authorization' = sprintf("Basic %s", Sys.getenv("KPLER_TOKEN"))
  )

  url <- "https://api.kpler.com/v1/flows"
  res_flows <- tryCatch({
    GET(url, query=params, add_headers(.headers = headers))
  }, error=function(e){
    # Try again
    return(GET(url, query=params, add_headers(.headers = headers)))
  })

  df <- readr::read_delim(content(res_flows, "raw"), delim=";", show_col_types=F)

  if(nrow(df)==0){return(NULL)}
  df %>%
    mutate(product=product,
           from=from,
           from_installation=from_installation,
           unit=unit) %>%
    tidyr::gather("split", "value", -Date, -from, -from_installation, -product, -unit, -`Period End Date`) %>%
    select(from,
           from_installation,
           split,
           product,
           date=Date,
           value,
           unit)
}

kpler.get_flows_raw <- function(froms=NULL,
                                products=NULL,
                                from_installations=NULL,
                                split =  "Destination Countries",
                                granularity = "daily",
                                unit = "t",
                                date_from = lubridate::today() - days(365),
                                date_to = lubridate::today()){

  fill <- function(x){if(is.null(x)){list(NULL)}else{x}}


  pblapply(fill(froms), function(from){
    lapply(fill(from_installations), function(from_installation){
      lapply(fill(products), function(product){
        kpler.get_flows_raw_single(
          from=from,
          from_installation = from_installation,
          product=product,
          split=split,
          granularity=granularity,
          date_from=date_from,
          date_to=date_to)}) %>%
        do.call(bind_rows, .)
    }) %>%
      do.call(bind_rows, .) }) %>%
    do.call(bind_rows, .)
}

kpler.get_products <- function(){
  c(1400, 1328, 1370)
  # c("Crude","Diesel","Gasoline","FO","Naphta","Gasoil", "Clean Products","Jet", "Condensate", "Others")
}

kpler.get_laundromat_flows <- function(use_cache=T){

  cache_filepath = 'cache/kpler_laundromat_flows.csv'

  if(file.exists(cache_filepath) & use_cache){
    return(read_csv(cache_filepath, col_types = list(unit = col_character())))
  }

  countries <- c("Russian Federation", "India", "Turkey", "Malaysia", "Egypt", "China", "United Arab Emirates")
  # products <- kpler.get_products()

  flows <- kpler.get_flows_raw(
    froms=countries,
    products=NULL,
    split="Products") %>%
    rename(product=split)

  write_csv(flows, cache_filepath)
  return(flows)
}

kpler.get_jamnagar_flows <- function(use_cache=T){

  products <- c("Crude","Diesel","Gasoline","FO","Naphta","Gasoil", "Clean Products","Jet", "Condensate", "Others") %>% URLencode

  flows <- pblapply(countries, function(country){
    lapply(products, function(x){
      kpler.get_flows_raw(
        from=country,
        product=x,
        split="Destination Countries")}) %>%
      do.call(bind_rows, .)
  }) %>%
    do.call(bind_rows, .) %>%
    rename(to=split)

  write_csv(flows, cache_filepath)
  return(flows)
}



kpler.get_trade <- function(imos=NULL, from_zones=NULL, date_from=NULL, date_to=NULL){

  readRenviron(".Renviron")

  library(lubridate)
  library(httr)
  library(pbapply)
  library(tidyverse)

  headers = c(
    'Authorization' = sprintf("Basic %s", Sys.getenv("KPLER_TOKEN"))
  )

  url <- "https://api.kpler.com/v1/trades"

  get_single_imo <- function(imo){
    params = list(
      vessels = paste(imo, collapse=","),
      fromZones=from_zones,
      startDate = date_from,
      endDate = date_to,
      size=10000)

    params <- Filter(Negate(is.null), params)

    res_trades <- GET(url, query=params, add_headers(.headers = headers))
    readr::read_delim(content(res_trades, "raw"), delim=";") %>%
      mutate(imo=imo)
  }

  if(!is.null(imos)){
    return(lapply(imos, get_single_imo))
  }else{
    return(get_single_imo(NULL))
  }
}
