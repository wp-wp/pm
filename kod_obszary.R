library(data.table)
library(sp)

setwd("...")
readRDS("airly_input.RDS")-> airly_input

#funkcja wyciagajaca id,lon,lat

ill <- function(x){
  data.frame(
    id=x$id,
    lon=x$loc$longitude,
    lat=x$loc$latitude
  )
}

#df nowe_id/lon/lat
lapply(airly_input$sensors, ill) -> id
rbindlist(id) -> id

#spatial df (coords+id+lon+lat)
coords = cbind(id$lon,id$lat)
sp_id <- SpatialPointsDataFrame(coords = coords, 
                               data = id,
                               proj4string = CRS("+proj=longlat"))

sp_id <- spTransform(sp_id, CRS("+init=epsg:2180"))


#spatial df (tylko coords+id)
coordinates(id) <- 2:3
proj4string(id) <- CRS("+proj=longlat")
id <- spTransform(id, CRS("+init=epsg:2180"))

#wizualizacja sensorow na mapie
library(ggmap)
library(mapview)
register_google(key = '...')
map <- get_map(location = 'Poland', zoom = 7)
mapview(sp_id)

#skrajne wartości 
# Warszawa
W= 20.84
N= 52.325
E= 21.1
S= 52.15

#dataframe dla prostokata otaczajacego Warszawe
subset(sp_id, lon>W & lon<E & lat>S & lat< N) -> WAW_data
mapview(WAW_data)

#Prostokat otaczajacy Warszawe
WAW_box<-Polygon(cbind(c(W,W,E,E), c(S,N,N,S)))
WAW_box<- Polygons(list(WAW_box), ID="WAW")
WAW_box<- SpatialPolygons(list(WAW_box))
proj4string(WAW_box) <- CRS("+proj=longlat")
WAW_box <- spTransform(WAW_box, CRS("+init=epsg:2180"))
mapview(WAW_box)
mapview(WAW_box)+mapview(WAW_box)

#skrajne wartości 
# Krakow

W= 19.85
N= 50.1
E= 20.05
S= 50

#dataframe dla prostokata otaczajacego Krakow
subset(sp_id, lon>W & lon<E & lat>S & lat< N) -> KR_data
mapview(KR_data)

#Prostokat otaczajacy Krakow
KR_box<-Polygon(cbind(c(W,W,E,E), c(S,N,N,S)))
KR_box<- Polygons(list(KR_box), ID="KR")
KR_box<-SpatialPolygons(list(KR_box))
proj4string(KR_box) <- CRS("+proj=longlat")
KR_box <- spTransform(KR_box, CRS("+init=epsg:2180"))
mapview(KR_box)
mapview(KR_box)+mapview(KR_data)
