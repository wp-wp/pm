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

fKrig <- function(city,raster){
  
  
}

########################################
###            OBLICZENIA            ### TEST DLA WARSZAWY PRIMARY ROADS
########################################

stacja <- readRDS(data_pmloc[2])                          ## DANE
z = readRDS("WAW/dist_mat.RDS")                           ## ODLEGLOSCI OD ULICY dla sensora
r = raster(data_dist[2])                                  ## ODLEGLOSCI OD ULICY raster
#z <- as.matrix(z)

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
  m_t  = t[14]
)
xy <- merge(xy,stacja.loc,by="m_id")


### DLA KONKRETNEJ DATY ###

xy.fill <- stacja[stacja$m_t %in% t[14],]

xy <- merge(xy, xy.fill[ , c("m_id", "pm10", "pm25")], by="m_id", all = TRUE)
xy$pm10[is.na(xy$pm10)] <- mean(xy$pm10, na.rm=TRUE)      ##zapelnienie srednimi wartosciami
xy$pm25[is.na(xy$pm25)] <- mean(xy$pm25, na.rm=TRUE)

x = cbind(xy$lon,xy$lat)                                  ## WSPOLRZEDNE STACJI
y10 = matrix(xy$pm10)                                     ## POZIOM PM10 (error qr.q2ty w/o matrix())
y25 = matrix(xy$pm25)                                     ## POZIOM PM25

smog10 <- Krig(x,y10,Z=z,theta = 100) 
smog25 <- Krig(x,y25,Z=z,theta = 100)

#########################################
surface(smog10)
surface(smog25)

