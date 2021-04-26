library(rgdal)
library(fields)
library(raster)

##################################
###            DANE            ###
##################################

city_list <- list.dirs(recursive = F, full.names = F)                                       ## LISTA FOLDEROW, FOLDER = MIASTO 
data_dist <- list.files("raster",pattern= "*_primary.grd", recursive = T, full.names = T)   ## LISTA RASTROW (.GRD)
data_pmloc <- list.files(pattern = "*_pmloc.RDS", recursive = T)                            ## DANE PM + LOKALIZACJA (.RDS)
street_list <- list.files("WAW/ulice/",pattern= "*.shp",full.names = F)

## street_level <- LISTA PLIKOW .SHP Z ULICAMI

#####################################
###            FUNKCJE            ###
#####################################


getZ <- function(city,street){
  points <- readOGR(paste0(city,"/pliki/", city,"_data.shp"))
  lines <- readOGR(paste0(city,"/ulice/", street))
  lines <- spTransform(lines, crs(points))
  
  dMat<- gDistance(lines,points, byid = T)
  minDist<- as.matrix(apply(dMat,1,min)) ##min distance/sensor
  
  raster <- raster(nrow=nrow(points), ext = extent(points), crs = crs(points))
  raster_points = as(raster,"SpatialPoints")
  distance_m <- gDistance(lines, raster_points, byid=TRUE) ##distance matrix
  sdf<- apply(distance_m,1,min)
  raster[] = sdf 
  names(raster) <- 'distance'
  writeRaster(raster, file= paste0("raster/", city,"_",street,".grd"))
  
  
  r <- raster(paste0("raster/", city,"_",street,".grd")) ##bez wczytania err(???)
  r.pop = raster("GEOSTAT.tif")
  r.proj = projectRaster(r.pop,r)
  pop <- extract(r.proj, points)
  
  z=cbind(minDist, pop)
  return(z)
}

simDF <- function(stacja){
  stacja.loc <- data.frame(                                 ## DATAFRAME LOKALIZACJI
    m_id=stacja$m_id,
    lon= stacja$lon,
    lat=stacja$lat
  )
  stacja.loc <- unique(stacja.loc)
  return(stacja.loc)
}

fKrig10 <- function(t, Z){
  stacja.dist <- cbind(stacja.loc,Z1=Z[,1], Z2= Z[,2])
  xy <- stacja[stacja$m_t %in% t,]
  xy <- merge(xy, stacja.dist[ , c("m_id", "Z1","Z2")], by="m_id")
  x = cbind(xy$lon,xy$lat)                                  
  y10 = matrix(xy$pm10)                                     
  z = cbind(xy$Z1, xy$Z2)
  z[is.na(z)] <- mean(xy$Z2,na.rm=TRUE)                 ##populacja ma 2 wartości NA - zapełniam je średnią
  smog10 <- Krig(x,y10,Z=z,theta = 50) 
  return(smog10)
}
fKrig25 <- function(t, z.all){
  stacja.dist <- cbind(stacja.loc,Z1=z.all[,1], Z2= z.all[,2])
  xy <- stacja[stacja$m_t %in% t,]
  xy <- merge(xy, stacja.dist[ , c("m_id", "Z1","Z2")], by="m_id")
  x = cbind(xy$lon,xy$lat)                                  
  y25 = matrix(xy$pm25)
  z = cbind(xy$Z1, xy$Z2)
  z[is.na(z)] <- mean(xy$Z2,na.rm=TRUE)                 ##populacja ma 2 wartości NA - zapełniam je średnią
  smog10 <- Krig(x,y10,Z=z,theta = 50) 
  return(smog10)
}

########################################
###            OBLICZENIA            ### 
########################################

#########################################TEST FUNKCJI
city_list<-"WAW"
street_list<-"primary.shp"
for(city in city_list){
  if (dir.exists(file.path(city, "pliki"))){
    stacja <- readRDS(paste0(city_list[1],"/",city_list[1],"_pmloc.RDS"))                          
    stacja.loc <- simDF(stacja)
    t.all = unique(stacja$m_t)                                   
    t.all <- sort(t.all, decreasing = F) 
    for (street in street_list) {
      Z = getZ(city_list[1],street_list[1])
      smog10_list <- vector("list", length(t.all))
      stacja.dist <- cbind(stacja.loc,Z1=Z[,1], Z2= Z[,2])
      for (t in 1:length(t.all)) {
          xy <- stacja[stacja$m_t %in% t.all[t],]
          xy <- merge(xy, stacja.dist[ , c("m_id", "Z1","Z2")], by="m_id")
          if (nrow(xy)>6){
            x = cbind(xy$lon,xy$lat)                                  
            y10 = matrix(xy$pm10)                                     
            z = cbind(xy$Z1, xy$Z2)
            z[is.na(z)] <- mean(xy$Z2,na.rm=TRUE)                 ##populacja ma 2 wartości NA - zapełniam je średnią
            try(smog10 <- Krig(x,y10,Z=z,theta = 50)) 
            if (!inherits(smog10, "try-error")){
              smog10_list[[t]]<-smog10
            } else {
              smog10_list[[t]]<-"Regression matrix for fixed part of model is colinear"
            }
          }else{
            smog10_list[[t]]<-"liczba pomiarów <7"
            }
      }
    }
  }
}
surface(smog10)
surface(smog10_list[[1]])

##funkcje-test

getZ <- function(city,street){
  points <- readOGR(paste0(city,"/pliki/", city,"_data.shp"))
  r <- raster(paste0("raster/", city,"_",street,".grd")) 
  dist <- extract(r,points) #get min dist
  r.pop = raster("GEOSTAT.tif")
  r.proj = projectRaster(r.pop,r)
  pop <- extract(r.proj, points) #get population
  z=cbind(Z1=dist, Z2=pop)
  
  return(z)
}

city = "WAW"
street = "primary.shp"

getDF <- function(stacja){
  stacja.loc <- data.frame(                                 ## df
    m_id=stacja$m_id,
    lon= stacja$lon,
    lat=stacja$lat
  )
  stacja.loc <- unique(stacja.loc)
  return(stacja.loc)
}

getT <- function(stacja){
  t.all = unique(stacja$m_t)                                   
  t.all <- sort(t.all, decreasing = F)
  return(t.all)
}

krig10 <- function(t,Z){
  stacja.dist <- cbind(stacja.loc,Z1=Z[,1], Z2= Z[,2])
  xy <- stacja[stacja$m_t %in% t,]
  xy <- merge(xy, stacja.dist[ , c("m_id", "Z1","Z2")], by="m_id")
  if (nrow(xy)>6){
    x = cbind(xy$lon,xy$lat)                                  
    y10 = matrix(xy$pm10)                                     
    z = cbind(xy$Z1, xy$Z2)
    z[is.na(z)] <- mean(xy$Z2,na.rm=TRUE)                 ##populacja ma 2 wartości NA - zapełniam je średnią
    try(smog10 <- Krig(x,y10,Z=z,theta = 50)) 
      if (!inherits(smog10, "try-error")){
        result<-smog10
        } else {
        result<-"Regression matrix for fixed part of model is colinear"
        }
     }else{
        result<-"liczba pomiarów <7"
    }
  return(result)
  }




for (city in city_list) {
  if (dir.exists(file.path(city, "pliki"))){ #warunek foldery
    stacja <- readRDS(paste0(city[1],"/",city[1],"_pmloc.RDS"))  #wczytanie danych sensorow
    stacja.loc <- getDF(stacja) #df zawierajaca id/lon/lat (w razie potrzeby zmienic lon/lat na e/n z innego pliku)
    t = getT(stacja)  #wektor z wartościami czasu
    for (street in street_list) {
      Z = getZ(city[1],street[1])
      krig_res<-krig10(t[14],Z)
  }
  }
}