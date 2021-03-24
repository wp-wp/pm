library(raster)
library(sf)
library(rgdal)

##wczytanie danych
setwd("")
points <- readOGR("KR_data.shp") 
setwd("")
readOGR("primary.shp")->lines

##sp to sf
points_sf<- st_as_sf(points)
lines_sf<- st_as_sf(lines)

points_sf = st_transform(points_sf, st_crs(lines_sf)) # check crs
grid <- st_make_grid(points_sf, cellsize = 300, what = "centers") ##create grid
ext <- extent(as(grid, "Spatial")) ##get extent

#create raster
raster <- raster(resolution = 200, ext = ext, crs = "+init=epsg:2180 +proj=longlat +units=m +no_defs")

require(rgeos)
raster_points <- as(r,"SpatialPoints") ##
distance_m <- gDistance(lines, raster_points, byid=TRUE) ##distance matrix
raster[] = apply(dd,1,min) ##fill raster w/ min value from each row
names(raster) <- 'distance'


