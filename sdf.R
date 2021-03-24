library(raster)
library(rgdal)

##wczytanie danych
setwd("")
points <- readOGR("KR_data.shp") 
setwd("")
readOGR("primary.shp")->lines

#create raster
raster <- raster(resolution = 200, ext = extent(points), crs = "+init=epsg:2180 +proj=longlat +units=m +no_defs")

require(rgeos)
raster_points = as(raster,"SpatialPoints")
distance_m <- gDistance(lines, raster_points, byid=TRUE) ##distance matrix
sdf<- apply(distance_m,1,min)
raster[] = sdf  ##or raster[]=apply(distance_m,1,min)
names(raster) <- 'distance'





