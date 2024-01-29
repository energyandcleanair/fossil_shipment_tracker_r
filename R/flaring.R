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

  return(fields %>% mutate(type='field'))
}

flaring.get_infrastructure <- function(){

  url <- "https://greeninfo-network.github.io/global-gas-infrastructure-tracker/data/data.csv?v=2.1"
  infra <- read_csv(url,
                    col_types = cols(lat = col_number(),
                                     lng = col_number())) %>%
    filter(grepl("Russia", countries))

  lines <- infra %>% filter(geom=='line', status=='operating')
  points <- infra %>% filter(geom=='point', (status=='operating') | grepl('Portovaya', project))

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
    mutate(geometry = list(sf::st_cast(route_to_linestring(route), "MULTILINESTRING"))) %>%
    sf::st_as_sf()

  sf::st_crs(lines_sf) <- 4326

  points_sf <- sf::st_as_sf(points, coords=c('lng','lat'))
  sf::st_crs(points_sf) <- 4326

  return(bind_rows(points_sf %>% mutate(type='point'),
                   lines_sf %>% mutate(type='pipeline')))
}

flaring.date_to_localpath <- function(date, ext='csv'){
  folder <- file.path(creahelpers::get_gis_dir(), 'fire', 'nvf')
  basename <- sprintf('nvf_%s.%s', strftime(as.Date(date),'%Y%m%d'),ext)
  return(file.path(folder, basename))
}

flaring.download_nvf_date <- function(date, force=F){

  library(httr)
  library(jsonlite)
  library(utils)
  library(R.utils)
  readRenviron(".Renviron")
  # Retrieve access token

  output_file <- flaring.date_to_localpath(date, ext='csv')
  output_file_gz <- flaring.date_to_localpath(date, ext='csv.gz')

  if(file.exists(output_file) & !force){
    return(TRUE)
  }

  params <- list(
    client_id = 'eogdata_oidc',
    client_secret = '2677ad81-521b-4869-8480-6d05b9e57d48',
    username = Sys.getenv('NVF_MINES_EMAIL'),
    password = Sys.getenv('NVF_MINES_PASSWORD'),
    grant_type = 'password'
  )
  token_url <- 'https://eogauth.mines.edu/auth/realms/master/protocol/openid-connect/token'
  response <- POST(token_url, body = params, encode = "form")
  access_token_list <- fromJSON(content(response,as="text",encoding="UTF-8"))
  access_token <- access_token_list$access_token

  # Submit request with token bearer and write to output file
  data_url <- sprintf('https://eogdata.mines.edu/wwwdata/viirs_products/vnf/v30//VNF_npp_d%s_noaa_v30-ez.csv.gz', strftime(as.Date(date),'%Y%m%d'))
  auth <- paste('Bearer', access_token)

  download.file(data_url, output_file_gz, mode = "wb", headers = list(Authorization = auth), quiet = TRUE)
  gunzip(output_file_gz, remove=T)
  return(file.exists(output_file))

}

flaring.download_nvf <- function(date_from, date_to){
  dates <- seq.Date(as.Date(date_from), as.Date(date_to), 'day')
  sapply(dates, flaring.download_nvf_date)
}

flaring.get_nvf <- function(date){
  nvf_file <- flaring.date_to_localpath(date)
  if(!file.exists(nvf_file)){
    flaring.download_nvf_date(date)
  }
  read_csv(nvf_file, col_types = cols())
}


flaring.get_flaring_amount <- function(date, geometries){

  tryCatch({
    geometries_sp <- creahelpers::to_spdf(geometries)
    flares_raw <- flaring.get_nvf(date=date)


    # Only keep relevant
    # https://www.mdpi.com/2072-4292/13/16/3078/htm
    # RH’=σT^4S^d
    sigma = 5.67E-8
    b1 = 0.0294
    d = 0.7

    flares <- flares_raw %>%
      filter(Temp_BB > 1200,
             Temp_BB < 999999) %>%
      mutate(
        rhp = sigma * Temp_BB^4 * Area_BB^d,
        bcm_est = b1 * rhp) %>%
      select(date=Date_LTZ, lon=Lon_GMTCO, lat=Lat_GMTCO, bcm_est)


    flares_sf <- sf::st_as_sf(flares, coords=c('lon', 'lat'))
    flares_sp <- as(flares_sf, "Spatial")

    suppressWarnings(sp::proj4string(flares_sp) <- sp::proj4string(geometries_sp))

    result <- tibble(geometries) %>% select(id, type) %>%
      left_join(
        cbind(
        as.data.frame(flares_sf) %>% select(-c(date, geometry)),
        sp::over(flares_sp, geometries_sp, returnList = F)) %>%
        filter(!is.na(id)) %>%
        group_by_at(setdiff(names(.), "bcm_est")) %>%
        summarise(
          bcm_est=sum(bcm_est),
          count=n()
        ) %>%
        ungroup()
      ) %>%
      mutate(bcm_est=tidyr::replace_na(bcm_est, 0),
             count=tidyr::replace_na(count, 0)) %>%
      mutate(date=!!date)
    return(result)
  }, error=function(e){
    warning(sprintf("Failed for date %s", date))
    return(NULL)
  })
}


flaring.get_flaring_ts <- function(gas_only=T,
                                   date_from="2018-01-01",
                                   date_to=lubridate::today()-3,
                                   buffer_km_fields=20,
                                   buffer_km_lines=10){

  fields <- flaring.get_fields(gas_only=gas_only)
  lines_points <- flaring.get_infrastructure()

  fields_buffer <- fields %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(buffer_km_fields*1000) %>%
    sf::st_transform(4326) %>%
    filter(!sf::st_is_empty(geometry))

  lines_points_buffer <- lines_points %>%
    sf::st_transform(3857) %>%
    sf::st_buffer(buffer_km_lines*1000) %>%
    sf::st_transform(4326) %>%
    filter(!sf::st_is_empty(geometry))

  geom_buffer <- bind_rows(
    lines_points_buffer %>% select(id=project, geometry, type),
    fields_buffer %>% select(id=clusterCaption,geometry, type)) %>%
    group_by(id) %>%
    summarise(geometry=sf::st_union(geometry))

  dates <- seq.Date(as.Date(date_from), as.Date(date_to), 'day')
  flare_amounts <- pbmcapply::pbmcmapply(flaring.get_flaring_amount, date=dates, geometries=list(geom_buffer),
                                     SIMPLIFY = F) %>%
    do.call(bind_rows, .)


  # Global tendencies
  flare_amounts %>%
    group_by(type, date) %>%
    summarise_at(c('bcm_est'), sum, na.rm=T) %>%
    rcrea::utils.running_average(14, vars_to_avg = c('bcm_est'), min_values = 10) %>%
    mutate(year=lubridate::year(date),
           date000=`year<-`(date, 2000)) %>%
    ggplot() +
      geom_line(aes(date000, bcm_est, col=factor(year))) +
    scale_x_date(date_labels = '%b') +
    scale_colour_brewer(palette='Reds', name=NULL) +
    rcrea::theme_crea() +
    facet_wrap(~type) +
    labs(title='Fire radiative power around Russian gas infrastructure',
         subtitle='14-day running average of radiative power within 20km of gas fields and 5km of gas infrastructure',
         y='MW',
         x=NULL,
         caption='Source: CREA analysis based on VIIRS, EnergyBase.ru and Global Energy Monitor.')

  ggsave('flaring.jpg', width=6, height=4, scale=1.5, dpi=150)


  # Top fields
  top_fields <- flare_amounts %>%
    filter(type=='pipeline') %>%
    group_by(id) %>%
    summarise(bcm_est=sum(bcm_est, na.rm=T)) %>%
    arrange(desc(bcm_est)) %>%
    pull(id) %>%
    head(20)


  flare_amounts %>%
    filter(id %in% top_fields) %>%
    rcrea::utils.running_average(14, vars_to_avg = c('bcm_est', 'count'),
                                 min_values=10) %>%
    mutate(year=lubridate::year(date),
           date000=`year<-`(date, 2000)) %>%
    ggplot() +
    geom_line(aes(date000, bcm_est, col=factor(year))) +
    scale_x_date(date_labels = '%b') +
    facet_wrap(~id, scales='free_y') +
    scale_colour_brewer(palette='Reds', name=NULL) +
    rcrea::theme_crea()


  # Top fields
  flare_amounts %>%
    # filter(type=='pipeline') %>%
    filter(id %in% 'Nord Stream Gas Pipeline') %>%
    # rcrea::utils.running_average(14, vars_to_avg = c('bcm_est')) %>%
    mutate(year=lubridate::year(date),
           date000=`year<-`(date, 2000)) %>%
    ggplot() +
    geom_bar(aes(date, bcm_est), stat='identity') +
    # scale_x_date(date_labels = '%b') +
    facet_wrap(~id, scales='free_y')

  dir.create('cache', F)
  saveRDS(flare_amounts, 'cache/flaring.RDS')

  return(flare_amounts)
}


flaring.detect_anomalies <- function(flare_amounts){

  library(anomalize)

  d <- flare_amounts %>%
    tidyr::complete(nesting(id, type), date, fill=list(bcm_est=0,
                                                       count=0))


 decomposed <- d %>%
    group_by(id) %>%
   group_map(function(x, id){
     print(head(id))
     x %>%
       # group_by(date=lubridate::floor_date(date, 'week')) %>%
       # summarise(bcm_est=sum(bcm_est, na.rm=T)) %>%
       rcrea::utils.running_average(14, vars_to_avg = c('bcm_est', 'count')) %>%
       filter(!is.na(bcm_est)) %>%
       time_decompose(bcm_est, frequency='1 year', trend='10 years') %>%
       anomalize(remainder, method = "iqr") %>%
       mutate(id=id$id)
   }) %>%
   do.call(bind_rows, .)

 # Anomalies lately
 top_anomalies <- decomposed %>%
   filter(date >= '2022-02-24') %>%
   filter(anomaly == 'Yes') %>%
   group_by(id) %>%
   summarise(n_anomalies=n()) %>%
   arrange(desc(n_anomalies)) %>%
   head(20) %>%
   select(id)


 data <- top_anomalies %>%
   left_join(decomposed) %>%
   mutate(id=factor(id, levels=top_anomalies$id))

  ggplot(data) +
    geom_line(aes(date, observed)) +
    geom_point(data=data %>% filter(anomaly=='Yes'),
               aes(date, observed, col='red')) +
   facet_wrap(~id,
              scales='free_y') +
   scale_y_continuous(limits=~c(0, quantile(.x,0.99))) +
    geom_vline(xintercept = as.Date('2022-02-24'), col='blue')


}

