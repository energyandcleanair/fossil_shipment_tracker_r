shipment.get_shipments <- function(){
  url <- "https://fossil-shipment-tracker.ew.r.appspot.com/v0/voyage?date_from=2021-11-01"
  shipments <- jsonlite::fromJSON(url)$data %>%
    filter(departure_iso2=="RU",
           arrival_iso2!="RU") %>%
    mutate_at(c("departure_date_utc","arrival_date_utc"), lubridate::as_date) %>%
    mutate(arrival_country = countrycode(arrival_iso2, "iso2c", "country.name"))
}
