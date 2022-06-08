api.get_voyages <- function(date_from=NULL, aggregate_by=NULL, rolling_days=NULL){
  url <- "https://api.russiafossiltracker.com/v0/voyage?format=csv&departure_iso2=RU&"

  if(!is.null(date_from)){
    url <- paste0(url, sprintf("date_from=%s&", strftime(date_from, "%Y-%m-%d")))
  }

  if(!is.null(aggregate_by)){
    url <- paste0(url, sprintf("aggregate_by=%s&", aggregate_by))
  }

  if(!is.null(rolling_days)){
    url <- paste0(url, sprintf("rolling_days=%s&", rolling_days))
  }

  readr::read_csv(url)
}

api.get_voyages_sf <- function(date_from=NULL, voyage_ids=NULL){
  # Can be quite slow
  url <- "https://api.russiafossiltracker.com/v0/voyage?date_from=2022-02-24&format=geojson&nest_in_data=False"
  # url <- "http://localhost:8080/v0/voyage?date_from=2022-02-24&format=geojson&nest_in_data=False"
  tryCatch({geojsonsf::geojson_sf(url) %>%
      filter(departure_iso2=="RU",
             (arrival_iso2!="RU" | is.na(arrival_iso2)))},
           error=function(e){
             return(NULL)
           })
}

api.get_berths_sf <- function(date_from=NULL){
  # Can be quite slow
  url <- sprintf("https://api.russiafossiltracker.com/v0/berth?format=geojson&nest_in_data=False")
  tryCatch({geojsonsf::geojson_sf(url(url))},
           error=function(e){
             return(NULL)
           })
}


api.get_voyage_line <- function(voyage_ids){
  voyage_id_param <- paste(voyage_ids, collapse=",")
  url <- sprintf("https://api.russiafossiltracker.com/v0/voyage?id=%d&format=geojson&nest_in_data=False", voyage_id_param)
  tryCatch({geojsonsf::geojson_sf(url(url))},
           error=function(e){
             return(NULL)
           })
}

api.get_voyage_points <- function(voyage_id){
  url <- sprintf("https://api.russiafossiltracker.com/v0/position?voyage_id=%d", voyage_id)
  positions <- jsonlite::fromJSON(url)$data %>%
    mutate_at(c("date_utc"), lubridate::as_date) %>%
    sf::st_as_sf(coords=c("lon","lat"))
}

api.get_portcalls <- function(date_from=NULL){
  url <- "https://api.russiafossiltracker.com/v0/portcall?"

  if(!is.null(date_from)){
    url <- paste0(url, sprintf("date_from=%s&", strftime(date_from, "%Y-%m-%d")))
  }

  jsonlite::fromJSON(url)$data
}


api.get_ports <- function(){
  url <- "https://api.russiafossiltracker.com/v0/port?"
  jsonlite::fromJSON(url)$data
}

api.get_ships <- function(){
  url <- "https://api.russiafossiltracker.com/v0/ship?"
  jsonlite::fromJSON(url)$data
}
