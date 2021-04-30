library(raster)
library(rgdal)
library(rgeos)

city_list <- list.dirs(recursive = F, full.names = F) ## folder=city

sdf<- function(city){
  points <- readOGR(paste0(city,"/pliki/", city,"_data.shp"))
  lines <- readOGR(paste0(city,"/pliki/", city,"_street.shp"))
  lines <- spTransform(lines, crs(points))
  raster <- raster(nrow=nrow(points), ext = extent(points), crs = crs(points))
  raster_points = as(raster,"SpatialPoints")
  distance_m <- gDistance(lines, raster_points, byid=TRUE) ##distance matrix
  sdf<- apply(distance_m,1,min)
  raster[] = sdf  ##or raster[]=apply(distance_m,1,min)
  names(raster) <- 'distance'
  writeRaster(raster, file= paste0("raster/", city,"_raster.grd"))
}

for(city in city_list){
  if (dir.exists(file.path(city, "pliki"))){
    sdf_type(city)
  }
}

## sdf dla 1 ulicy (primary) test (pliki w folderze test)
## wektor z nazwami ulic?

sdf_type<- function(city){
  points <- readOGR(paste0(city,"/pliki/", city,"_data.shp"))
  #lines <- readOGR(paste0(city,"/ulice_", city,"/primary.shp"))
  lines <- readOGR(paste0("/home/weronika/Desktop/test/",city,"/ulice_", city, "/secondary.shp"))
  lines <- spTransform(lines, crs(points))
  raster <- raster(nrow=nrow(points), ext = extent(points), crs = crs(points))
  raster_points = as(raster,"SpatialPoints")
  distance_m <- gDistance(lines, raster_points, byid=TRUE) ##distance matrix
  sdf<- apply(distance_m,1,min)
  raster[] = sdf  ##or raster[]=apply(distance_m,1,min)
  names(raster) <- 'distance'
  writeRaster(raster, file= paste0("raster/", city,"_secondary.shp.grd"))
  return(raster)
}

sdf_type(city_list[5])

