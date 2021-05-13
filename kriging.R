library(parallel)
library(rgdal)
library(fields)
library(raster)

city_list <- list.dirs(recursive = F, full.names = F)                                       ## LISTA FOLDEROW, FOLDER = MIASTO 
data_pmloc <- list.files(pattern = "*_pmloc.RDS", recursive = T)                            ## DANE PM + LOKALIZACJA (.RDS)
street_list <- list.files("WAW/ulice/",pattern= "*.shp",full.names = F)
city.list<- c("WAW","KR")
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

krig.inside.condition <- function(x,y10,z){                ## warunek na error
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
  
  stacja <- readRDS(paste0(city,"/",city,"_pmloc.RDS")) 
  stacja.loc <- getDF(stacja)
  
  z=cbind(Z1=dist, Z2=pop)
  
  stacja.dist <- cbind(stacja.loc,Z1=dist, Z2=pop)
  xy <- stacja[stacja$m_t %in% t,]
  xy <- merge(xy, stacja.dist[ , c("m_id", "Z1","Z2")], by="m_id")
  
  if (nrow(xy)>6){                                          ##warunek za zbyt mala liczbe pomiarow (mniej niz 7 = nie dziala Krig)
    x = cbind(xy$lon,xy$lat)                                  
    y10 = matrix(xy$pm10)                                     
    z = cbind(xy$Z1, xy$Z2)
    z[is.na(z)] <- mean(xy$Z2,na.rm=T)                 ##populacja ma 2 wartości NA - zapełniam je średnią
    result<- krig.inside.condition(x,y10,z)
  }else{
    result<-"liczba pomiarów jest zbyt mała (<7)"
  }
  return(result)
}

street.loop <- function(city,street){
  stacja <- readRDS(paste0(city,"/",city,"_pmloc.RDS")) ## ponowne ladowanie danych aby wyciagnąc t
  t = getT(stacja) #t.test
  smog.list <- mclapply(t, krig.outside.loop, city=city, street=street,mc.cores=90)
  
  saveRDS(smog.list,file=paste0(city,"/",city,"_kriging_",street,".RDS"))
  #return(paste0("krig policzony dla ", street, city)
}

city.loop <- function(city){
  if (dir.exists(file.path(city, "pliki"))){ 
    lapply(street_list,street.loop, city=city)->street.krig
    #names(street.krig)<-street.test
    #return(street.krig)
  }
}

########################################
###            OBLICZENIA            ### 
########################################
ptm <- proc.time()
lapply(city_list,city.loop) -> krig.all   ##zapisuje bezposrednio do plikow dla miast
#mclapply(city_list,city.loop, mc.cores = 96) #wersja na epyc
proc.time() - ptm


