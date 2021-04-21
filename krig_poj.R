city_list <- list.dirs(recursive = F, full.names = F)                                       ## LISTA FOLDEROW, FOLDER = MIASTO 
data_dist <- list.files("raster",pattern= "*_primary.grd", recursive = T, full.names = T)   ## LISTA RASTROW (.GRD)
data_pmloc <- list.files(pattern = "*_pmloc.RDS", recursive = T)                            ## DANE PM + LOKALIZACJA (.RDS)
street_list <- list.files("WAW/ulice/",pattern= "*.shp",full.names = F)

stacja <- readRDS(data_pmloc[2])                          ## DANE
z1 = readRDS("WAW/dist_mat.RDS")                           ## ODLEGLOSCI OD ULICY dla sensora


r = raster(data_dist[2])                                  ## ODLEGLOSCI OD ULICY raster
r.pop = raster("GEOSTAT.tif")
r.proj = projectRaster(r.pop,r)
shp = readOGR("WAW/pliki/WAW_data.shp")
population <- extract(r.proj, shp)


stacja.loc <- data.frame(                                 ## DATAFRAME LOKALIZACJI
  m_id=stacja$m_id,
  lon= stacja$lon,
  lat=stacja$lat
) 

stacja.loc <- unique(stacja.loc) 
stacja.dist <- cbind(stacja.loc,Z1=z1, Z2= population)
t = unique(stacja$m_t)                                    ## CZAS
t <- sort(t, decreasing = F) 

### DLA KONKRETNEJ DATY ###

xy <- stacja[stacja$m_t %in% t[14],]

xy <- merge(xy, stacja.dist[ , c("m_id", "Z1","Z2")], by="m_id")

if (length(xy)<7) stop('liczba pomiarów mniejsza niż 7')

x = cbind(xy$lon,xy$lat)                                  ## WSPOLRZEDNE STACJI
y10 = matrix(xy$pm10)                                     ## POZIOM PM10 (error qr.q2ty w/o matrix())
y25 = matrix(xy$pm25)                                     ## POZIOM PM25
z = cbind(xy$Z1, xy$Z2)

smog10 <- Krig(x,y10,Z=z,theta = 100) 
smog25 <- Krig(x,y25,Z=z,theta = 100)

smog10.pred <- predict(smog10)
surface(smog10)
