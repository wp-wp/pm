library(rgdal)
library(fields)
library(raster)

##################################
###            DANE            ###
##################################

city_list <- list.dirs(recursive = F, full.names = F)                                       ## LISTA FOLDEROW, FOLDER = MIASTO 
data_pmloc <- list.files(pattern = "*_pmloc.RDS", recursive = T)                            ## DANE PM + LOKALIZACJA (.RDS)
street_list <- list.files("WAW/ulice/",pattern= "*.shp",full.names = F)

#####################################
###            FUNKCJE            ###
#####################################

getDF <- function(stacja){
  stacja.loc <- data.frame(                                 ## df z danymi lokalizacji
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

krig.inside.condition <- function(x,y10,z){ #działa
  try(smog10 <- Krig(x,y10,Z=z,theta = 50)) 
  if (!inherits(smog10, "try-error")){
    result<-smog10
  } else {
    result<-"Regression matrix for fixed part of model is colinear"
  }
  return(result)
}

krig.outside.loop <- function(city,street,t){ #działa
  points <- readOGR(paste0(city,"/pliki/", city,"_data.shp"))
  r <- raster(paste0("raster/", city,"_",street,".grd")) 
  dist <- extract(r,points) #get min dist
  r.pop = raster("GEOSTAT.tif")
  r.proj = projectRaster(r.pop,r)
  pop <- extract(r.proj, points) #get population
  
  z=cbind(Z1=dist, Z2=pop)
  
  stacja.dist <- cbind(stacja.loc,Z1=dist, Z2=pop)
  xy <- stacja[stacja$m_t %in% t,]
  xy <- merge(xy, stacja.dist[ , c("m_id", "Z1","Z2")], by="m_id")
  
  if (nrow(xy)>6){
    x = cbind(xy$lon,xy$lat)                                  
    y10 = matrix(xy$pm10)                                     
    z = cbind(xy$Z1, xy$Z2)
    z[is.na(z)] <- mean(xy$Z2,na.rm=TRUE)                 ##populacja ma 2 wartości NA - zapełniam je średnią
    result<- krig.inside.condition(x,y10,z)
  }else{
    result<-"liczba pomiarów <7"
  }
  return(result)
}

street.loop <- function(street.level){ 
  smog10.street<- mapply(krig.outside.loop, t.test, street=street.level, city=city)#######
  return(smog10.street)
}

########################################
###            OBLICZENIA            ### 
########################################

city = "KR"
city.list <- c("WAW","KR")
street.test <- c("primary.shp")

##dziala
for (city in city.list) {
  if (dir.exists(file.path(city, "pliki"))){ 
    stacja <- readRDS(paste0(city,"/",city,"_pmloc.RDS")) 
    stacja.loc <- getDF(stacja)
    t = getT(stacja)
    t.test <- t[14:18]
    lapply(street.test,street.loop)->smog.test2 

  }
}

city.loop <- function(city){
  stacja <- readRDS(paste0(city,"/",city,"_pmloc.RDS")) 
  stacja.loc <- getDF(stacja)
  t = getT(stacja)
  smog.list <- lapply(street.test,street.loop)###
  saveRDS(smog.list,file=paste0(city,"/",city,"_krig.RDS"))
}

city.loop(city)->testing.fun
lapply(city.list,city.loop)->test.loop
