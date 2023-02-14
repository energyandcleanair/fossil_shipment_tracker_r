kpler.get_flows <- function(use_cache=T){


  cache_filepath = 'cache/kpler_flows.csv'

  if(file.exists(cache_filepath) & use_cache){
    return(read_csv(cache_filepath))
  }

  readRenviron(".Renviron")

  library(glue)
  library(lubridate)
  library(httr)
  library(pbapply)
  library(tidyverse)


  countries <- c("Russian Federation", "India", "Turkey", "Malaysia", "Egypt", "China", "United Arab Emirates")
  products <- c("Crude","Diesel","Gasoline","FO","Naphta","Gasoil","Clean Products","Jet", "Fuel Oils", "Crude/Co", "Others") %>% URLencode

  get_flows <- function(country, product){
    params = list(
      fromZones = country,
      products = product,
      flowDirection = "Export",
      split =  "Destination Countries",
      granularity = "daily",
      startDate = lubridate::today() - days(365),
      endDate = lubridate::today(),
      unit = "t",
      withForecast = "false",
      withIntraCountry = "false")

    headers = c(
      'Authorization' = sprintf("Basic %s", Sys.getenv("KPLER_TOKEN"))
    )

    res_flows <- GET(url, query=params, add_headers(.headers = headers))
    readr::read_delim(content(res_flows, "raw"), delim=";") %>%
      mutate(product=product,
             country=country)
  }


  flows <- pblapply(countries, function(country){
    lapply(products, function(x){get_flows(country=country, product=x)}) %>%
      do.call(bind_rows, .)
  }) %>%
    do.call(bind_rows, .)

  # Wide -> long
  flows <- flows %>%
    tidyr::gather("destination_country", "value_tonne", -Date, -country, -product) %>%
    select(origin_country=country,
           destination_country,
           product,
           date=Date,
           value_tonne)

  write_csv(flows, cache_filepath)
  return(flows)
}
