library(rgdal)
library(fields)
library(raster)
library(rgeos)

##################################
###            DANE            ###
##################################

city_list <- list.dirs(recursive = F, full.names = F)                                       ## LISTA FOLDEROW, FOLDER = MIASTO 
data_dist <- list.files("raster",pattern= "*_primary.grd", recursive = T, full.names = T)   ## LISTA RASTROW (.GRD)
data_pmloc <- list.files(pattern = "*_pmloc.RDS", recursive = T)                            ## DANE PM + LOKALIZACJA (.RDS)
## street_level <- LISTA PLIKOW .SHP Z ULICAMI

#####################################
###            FUNKCJE            ###
#####################################

sdf<- function(city){                                                                       ## FUNKCJA (MIASTO, ULICA)
  points <- readOGR(paste0(city,"/pliki/", city,"_data.shp"))                               ## DO STWORZENIA .SHP ULIC (LISTA steet_level)
  lines <- readOGR(paste0(city,"/ulice_", city,"/primary.shp"))                             ## LUB OBLICZENIE RASTROW OSOBNO
  lines <- spTransform(lines, crs(points))
  raster <- raster(resolution = 200, ext = extent(WAW_raster), crs = crs(points))
  raster_points = as(raster,"SpatialPoints")
  distance_m <- gDistance(lines, raster_points, byid=TRUE) ##distance matrix
  sdf<- apply(distance_m,1,min)
  raster[] = sdf  ##or raster[]=apply(distance_m,1,min)
  names(raster) <- 'distance'
  return(raster)
}

fKrig <- function(city,raster){
  
  
}

########################################
###            OBLICZENIA            ### TEST DLA WARSZAWY PRIMARY ROADS
########################################

stacja <- readRDS(data_pmloc[2])                          ## DANE
Z = raster(data_dist[2])                                  ## ODLEGLOSCI OD ULICY

t = unique(stacja$m_t)                                    ## CZAS
t <- sort(t, decreasing = F)  

### DLA KONKRETNEJ DATY ###

XY <- stacja[stacja$m_t %in% t[5],]
coordinates(XY) <- 5:6
crs(XY) <- crs(Z)

r <- raster(extent(Z), crs=crs(Z))

X = cbind(XY$lon,XY$lat)                                  ## WSPOLRZEDNE STACJI
Y10 = matrix(XY$pm10)                                     ## POZIOM PM10 (error qr.q2ty w/o matrix())
Y25 = matrix(XY$pm25)                                     ## POZIOM PM25

smog10 <- Krig(X,Y10, Z=)
smog25 <- Krig()

#########################################



