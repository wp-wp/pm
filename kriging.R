library(rgdal)
library(fields)
library(raster)

city_list <- list.dirs(recursive = F, full.names = F) ## folder=city
data_dist <- list.files("raster",pattern= "*_primary.grd", recursive = T, full.names = T) ## raster Z
data_pmloc <- list.files(pattern = "*_pmloc.RDS", recursive = T) ## smog Y


WAW_raster <- raster(data_dist[2]) 
WAW_pmloc <- readRDS(data_pmloc[2])

time <- unique(WAW_pmloc$m_t)
time<- sort(time, decreasing = F)
pmloc <- WAW_pmloc[WAW_pmloc$m_t %in% time[1],]

pm = pmloc$pm10
loc = data.frame(lon=pmloc$lon, lat=pmloc$lat)

raster <- 
  
smog <- Krig()


fKrig <- function(city){
  
}

sdf<- function(city, pmloc){
  points <- pmloc
  coordinates(points) <- 3:4
  points <- spTransform(points, crs=("+proj=tmerc +lat_0=0 +lon_0=19 +k=0.9993 +x_0=500000 +y_0=-5300000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m
+no_defs"))
  lines <- readOGR(paste0(city,"/ulice_", city,"/primary.shp"))
  lines <- spTransform(lines, crs(points))
  raster <- raster(resolution = 200, ext = extent(points), crs = crs(points))
  raster_points = as(raster,"SpatialPoints")
  distance_m <- gDistance(lines, raster_points, byid=TRUE) ##distance matrix
  sdf<- apply(distance_m,1,min)
  raster[] = sdf  ##or raster[]=apply(distance_m,1,min)
  names(raster) <- 'distance'
  return(raster)
}

