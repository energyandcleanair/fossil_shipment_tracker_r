flaring.get_fields <- function(gas_only=T){

  # 403: need to download the file manually
  # url <- "https://api.energybase.ru/v1/map-feature/field"
  # geojsonsf::geojson_sf(url)

  fields <- geojsonsf::geojson_sf("data/russia_oil_gas_fields.geojson")
  # Coordinates are reversed
  coords <- sf::st_coordinates(fields)
  new_geometry <- sf::st_as_sf(as.data.frame(coords), coords=c("Y","X"))
  sf::st_geometry(fields) <- new_geometry$geometry
  sf::st_crs(fields) <- 4326

  if(gas_only){
    features <- jsonlite::read_json("data/russia_oil_gas_fields.geojson")[[2]]
    has_gas <- unlist(lapply(features, function(x) grepl("gas", x$options$iconImageHref)))
    fields <- fields[has_gas,]
  }
  return(fields)
}

flaring.get_infrastructure <- function(){
  url <- "https://greeninfo-network.github.io/global-gas-infrastructure-tracker/data/data.csv?v=2.1"
  infra <- read_csv(url) %>%
    filter(grepl("Russia", countries))

  lines <- infra %>% filter(geom=='line', status=='operating')
  points <- infra %>% filter(geom=='point', status=='operating')

  route_to_linestring <- function(route){
    tryCatch({
      multiline <- grepl(";", route)
      if(multiline){
        return(sf::st_multilinestring(lapply(stringr::str_split(route, ";")[[1]], route_to_linestring)))
      }
      coords <- stringr::str_split(route, ":")[[1]] %>%
        lapply(function(x) stringr::str_split(x, ",")[[1]] %>% as.numeric)

      t(matrix(unlist(coords),nrow=2))[,2:1] %>%
        sf::st_linestring(dim='YX')
    }, error=function(e){
      print(route)
      return(NA)
    })
  }

  lines_sf <- lines %>%
    rowwise() %>%
    mutate(geometry = list(st_cast(route_to_linestring(route), "MULTILINESTRING"))) %>%
    sf::st_as_sf()

  sf::st_crs(lines_sf) <- 4326

  return(lines_sf)
}


flaring.get_flaring_ts <- function(gas_only=T,
                                   date_from="2018-01-01",
                                   date_to=lubridate::today(),
                                   buffer_km_fields=20,
                                   buffer_km_lines=5){

  fields <- flaring.get_fields(gas_only=gas_only)
  lines <- flaring.get_infrastructure()

  fields_buffer <- fields %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(buffer_km_fields*1000) %>%
    sf::st_transform(4326) %>%
    filter(!sf::st_is_empty(geometry))

  lines_buffer <- lines %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(buffer_km_lines*1000) %>%
    sf::st_transform(4326) %>%
    filter(!sf::st_is_empty(geometry))

  geom_buffer <- bind_rows(lines_buffer %>% select(id=project,
                                    geometry),
            fields_buffer %>% select(id=clusterCaption,geometry))

  fires <- creatrajs::fire.aggregate(date_from=date_from, date_to=date_to,
                                     geometries = geom_buffer)

  fires %>%
    group_by(date) %>%
    summarise_at(c('frp','count'), sum, na.rm=T) %>%
    rcrea::utils.running_average(14, vars_to_avg = c('frp','count')) %>%
    mutate(year=lubridate::year(date),
           date000=`year<-`(date, 2000)) %>%
    ggplot() +
      geom_line(aes(date000, frp, col=factor(year))) +
    scale_x_date(date_labels = '%b') +
    scale_colour_brewer(palette='Reds', name=NULL) +
    rcrea::theme_crea() +
    labs(title='Fire radiative power around Russian gas infrastructure',
         subtitle='14-day running average of radiative power within 20km of gas fields and 5km of gas infrastructure',
         y='MW',
         x=NULL,
         caption='Source: CREA analysis based on VIIRS, EnergyBase.ru and Global Energy Monitor.')

  ggsave('flaring.jpg', width=6, height=4, scale=1.5, dpi=150)

  top_fields <- fires %>%
    group_by(clusterCaption) %>%
    summarise(frp=sum(frp, na.rm=T)) %>%
    arrange(desc(frp)) %>%
    pull(clusterCaption) %>%
    head(20)


  fires %>%
    filter(clusterCaption %in% top_fields) %>%
    rcrea::utils.running_average(14, vars_to_avg = c('frp','count')) %>%
    mutate(year=lubridate::year(date),
           date000=`year<-`(date, 2000)) %>%
    ggplot() +
    geom_line(aes(date000, frp, col=factor(year))) +
    scale_x_date(date_labels = '%b') +
    facet_wrap(~clusterCaption, scales='free_y')

}

