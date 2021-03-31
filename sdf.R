library(raster)
library(rgdal)
library(rgeos)

##wczytanie danych
setwd("")
points <- readOGR("KR_data.shp") 
lines <- readOGR("street_KR.shp")

##CRS!!!
points <- spTransform(points, CRS("+init=epsg:2180 +proj=longlat +units=m +no_defs"))
lines <- spTransform(lines, CRS("+init=epsg:2180 +proj=longlat +units=m +no_defs"))


#raster
raster <- raster(resolution = 200, ext = extent(points), crs = "+init=epsg:2180 +proj=longlat +units=m +no_defs")
raster_points = as(raster,"SpatialPoints")
distance_m <- gDistance(lines, raster_points, byid=TRUE) ##distance matrix
sdf<- apply(distance_m,1,min)
raster[] = sdf  ##or raster[]=apply(distance_m,1,min)
names(raster) <- 'distance'


dToStreet<- function(points,lines){
  raster <- raster(resolution = 200, ext = extent(points), crs = "+init=epsg:2180 +proj=longlat +units=m +no_defs")
  raster_points = as(raster,"SpatialPoints")
  distance_m <- gDistance(lines, raster_points, byid=TRUE) ##distance matrix
  sdf<- apply(distance_m,1,min)
  raster[] = sdf  ##or raster[]=apply(distance_m,1,min)
  names(raster) <- 'distance'
  
  return(raster)
  }

save(raster, file="r.RData")

