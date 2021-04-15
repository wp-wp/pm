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
z = raster(data_dist[2])                                  ## ODLEGLOSCI OD ULICY

stacja.loc <- data.frame(                                 ## DATAFRAME LOKALIZACJI
  m_id=stacja$m_id,
  lon= stacja$lon,
  lat=stacja$lat
) 

stacja.loc <- unique(stacja.loc) 

t = unique(stacja$m_t)                                    ## CZAS
t <- sort(t, decreasing = F) 


xy <- data.frame(                                    ## DATAFRAME DLA JEDNEJ GODZINY
  m_id = unique(stacja$m_id),
  m_t  = t[5], ## tu petla/funkcja
  pm10 = NA,
  pm25 = NA
)
xy <- merge(xy,stacja.loc,by="m_id")


### DLA KONKRETNEJ DATY ###

xy.fill <- stacja[stacja$m_t %in% t[5],]

fill.fun <- function ## uzupelnienie pm10,pm25

lapply(xy, fill.fun)->xy  



x = cbind(xy$lon,xy$lat)                                  ## WSPOLRZEDNE STACJI
y10 = matrix(xy$pm10)                                     ## POZIOM PM10 (error qr.q2ty w/o matrix())
y25 = matrix(xy$pm25)                                     ## POZIOM PM25

smog10 <- Krig(x,y10,theta = 100,m = 1) #z nrow = xy nrow ok
smog25 <- Krig()

#########################################
plot(smog10)
surface(smog10)
plot(xy, add=T)
