# Plotting map of voyages and pipelines for the 6-month report

library(ggplot2)
library(tidyverse)
library(sf)
library(geojsonsf)
library(lubridate)
library(spatstat)
require(sp)
require(raster)
require(maptools)
require(spatstat)
library(basemaps)
library(mapboxapi)
library(ggspatial)
library(RColorBrewer)
library(ggmap)
library(magick)
library(pbapply)


map.generate_dates <- function(trajs=NULL,
                                sigma=30000,
                                eps=10000,
                                date_from='2022-02-24',
                                date_to='2022-08-24',
                                bounds=NULL,
                                filepath='map/density_final.jpg'){

  if(is.null(trajs)) {trajs <- map.get_trajs(use_cache=T)}
  running_days <- 7
  dates <- seq.Date(as.Date('2022-02-24') - lubridate::days(running_days),
                    as.Date('2022-08-24'), by='day')

  densities <- pblapply(dates,
                      function(date){
                        trajs_date <- trajs %>% filter(lubridate::floor_date(as.Date(arrival_date_utc))==date)
                        return(map.get_density(trajs=trajs, sigma=sigma, eps=eps))
                      })

  terra::writeRaster(density,
                     sprintf('map/density_sigma%d_eps%d.tif',sigma,eps),
                     overwrite=T)

  density_8bit = map.stretch(density, bounds=bounds)
  terra::writeRaster(density_8bit,
                     sprintf('map/density_8bit_sigma%d_eps%d.tif',sigma,eps),
                     overwrite=T,
                     datatype='INT1U')
  bm <- map.get_basemap()
  scale <- map.get_scale(filename='density_scale_crea.txt')
  map.plot(bm=bm,
           density_8bit=density_8bit,
           scale=scale,
           subtitle=sprintf('%s to %s',
                            strftime(as.Date(date_from),'%d %B'),
                            strftime(as.Date(date_to),'%d %B %Y')),

           filepath=filepath)
}


map.generate_single <- function(trajs=NULL,
                         sigma=30000,
                         eps=10000,
                         date_from='2022-02-24',
                         date_to='2022-08-24',
                         bounds=NULL,
                         filepath='map/density_final.jpg'){

  if(is.null(trajs)) {trajs <- map.get_trajs()}

   density <- map.get_density(trajs=trajs, sigma=sigma, eps=eps)
   terra::writeRaster(density,
                      sprintf('map/density_sigma%d_eps%d.tif',sigma,eps),
                      overwrite=T)

   density_8bit = map.stretch(density, bounds=bounds)
   terra::writeRaster(density_8bit,
                      sprintf('map/density_8bit_sigma%d_eps%d.tif',sigma,eps),
                      overwrite=T,
                      datatype='INT1U')
   bm <- map.get_basemap(trajs=trajs)
   scale <- map.get_scale(filename='density_scale_crea.txt')

   map.plot(bm=bm,
            density_8bit=density_8bit,
            scale=scale,
            title="Russian fossil fuel shipments",
            subtitle=sprintf('%s to %s',
                             strftime(as.Date(date_from),'%d %B'),
                             strftime(as.Date(date_to),'%d %B %Y')),

            filepath=filepath)

   map.plot(bm=bm,
            density_8bit=density_8bit,
            scale=scale,
            title="Russian fossil fuel shipments",
            width=6000,
            subtitle=sprintf('%s to %s',
                             strftime(as.Date(date_from),'%d %B'),
                             strftime(as.Date(date_to),'%d %B %Y')),

            filepath==gsub("\\.", "_hd\\.",filepath))

   map.plot(bm=bm,
            density_8bit=density_8bit,
            scale=scale,
            filepath=gsub("\\.", "_notitle\\.",filepath))

   map.plot(bm=bm,
            density_8bit=density_8bit,
            scale=scale,
            width=6000,
            filepath=gsub("\\.", "_notitle_hd\\.",filepath))
}


map.get_trajs <- function(use_cache=T){
  file <- 'map/trajs_3857.gpkg'
  if(use_cache && file.exists(file)){
    return(sf::read_sf(file))
  }

  urls <- sprintf("https://api.russiafossiltracker.com/v0/voyage?date_from=2022-%02d-17&date_to=2022-%02d-24&format=geojson&nest_in_data=False&routed_trajectory=True&commodity_group=oil,coal,gas", seq(2,7), seq(3,8))

  trajs <- lapply(urls, function(url){geojsonsf::geojson_sf(url) %>%
      dplyr::select(departure_iso2, arrival_iso2, commodity, commodity_group, geometry, ship_dwt, departure_date_utc, arrival_date_utc)}) %>%
    do.call(bind_rows, .) %>%
    filter(departure_iso2 == "RU",
           arrival_iso2 != "RU")

  trajs <- trajs %>% filter(arrival_date_utc<='2022-08-23')
  trajs$month <- month(trajs$arrival_date_utc) + (day(trajs$arrival_date_utc) >= 24)
  trajs$month_str <- sprintf('2022-%02d-24 to 2022-%02d-23', trajs$month-1, trajs$month)
  trajs <- trajs %>% filter(!sf::st_is_empty(geometry))
  trajs_3857 <- trajs[!sf::st_is_empty(trajs$geometry),] %>% sf::st_transform(crs=3857)
  sf::write_sf(trajs_3857, file)
  return(trajs_3857)
}

map.get_density <- function(trajs, sigma=20000, eps=50000){
  pspSl <- as.psp(trajs)
  selected_sigma <- sigma
  px <- pixellate(pspSl, eps=eps)
  px_blurred <- blur(px, sigma=selected_sigma)
  density <- raster(px_blurred)
  raster::crs(density) <- 3857
  return(density)
}

map.stretch <- function (x, bounds=NULL) {
  y <- sqrt(clamp(x, 1))
  if(is.null(bounds)){
    bounds <- stats::quantile(y, c(0.8, 1), na.rm = TRUE)
  }
  temp <- calc(y, fun = function(x) (255 * (x - bounds[1]))/(bounds[2] - bounds[1]))
  temp[temp < 0] <- 0
  temp[temp > 254] <- 254
  return(temp)
}



map.get_basemap <- function(trajs, use_cache=F){
  file <- 'map/basemap.tif'
  if(use_cache && file.exists(file)){
    return(raster::raster(file))
  }

  bm <- get_static_tiles(
    location=st_as_sfc(st_bbox(trajs)),
    zoom=2,
    'cl7dl6clf000m14qw5jwzev61',
    username='hubert-thieriot',
    style_url = NULL,
    scaling_factor = c("2x"),
    buffer_dist = 60000,
    units = "m",
    crop = T,
    access_token = Sys.getenv('MAPBOX_TOKEN')
  )
  terra::writeRaster(bm, 'map/basemap.tif', overwrite=T, datatype='INT1U')
  return(bm)
}

map.get_scale <- function(filename='density_scale.txt'){
  read_csv(file.path('map', filename), skip=2,col_names=c('value','r','g','b')) %>%
    mutate(colour=rgb(r,g,b, maxColorValue=255)) %>%
    dplyr::select(value, colour)
}


map.plot <- function(bm, density_8bit, scale,
                     title='',
                     subtitle='',
                     width=NULL,
                     filepath='map/density_final.jpg'){

   rgb <- raster::RGB(density_8bit,
                      col=scale$colour,
                      breaks=scale$value,
                      colNA='transparent',
                      alpha=T)

  # Default function only uses discrete colors
   # We use this to have nicer ones
  library(scales)
  library(circlize)
  col_fun = colorRamp2(scale$value, scale$colour)
  a=col_fun(density_8bit[])
  b=col2rgb(a)
  rgb$red[] = b['red',]
  rgb$green[] = b['green',]
  rgb$blue[] = b['blue',]


  rgb$alpha[density_8bit==0] <- 0
  rgb_terra <- terra::rast(rgb)
  rgb_terra$alpha <- terra::rast(raster(rgb, layer=4) / 255)

  hw_ratio <- dim(density_8bit)[1] / dim(density_8bit)[2]
  if(is.null(width)) width <- dim(density_8bit)[2]
  height <- width * hw_ratio


  fig <- image_graph(width=width, height=height)
  terra::plotRGB(bm)
  terra::plotRGB(rgb_terra, a=4, add=T)
  dev.off()

  logo <- magick::image_read_svg('https://energyandcleanair.org/wp/wp-content/uploads/2022/08/CREA-logo.svg')

  fig_annotated <- image_annotate(fig, "© Mapbox | © CREA",
                 size = 30,
                 color = "#35416C",
                 boxcolor = "#FFFFFF08",
                 gravity="SouthEast",
                 location = "+30+30") %>%
    image_annotate(title,
                   size = 70,
                   weight='600',
                   color = "#35416C",
                   # boxcolor = "#FFFFFF08",
                   gravity="Northwest",
                   location = "+20+20") %>%
    image_annotate(subtitle,
                   size = 40,
                   color = "#35416C",
                   # boxcolor = "#FFFFFF08",
                   gravity="Northwest",
                   location = "+20+100") %>%
    image_composite(image_resize(logo, '300'), offset = "+30+30", gravity='SouthWest')

  image_write(fig_annotated, filepath, format='jpg', quality=100)
}

