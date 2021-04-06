library(data.table)
library(sp)
library(rgdal)

readRDS("airly_input.RDS")-> airly_input

ill <- function(x){
  data.frame(
    id=x$id,
    lon=x$loc$longitude,
    lat=x$loc$latitude
  )
}

lapply(airly_input$sensors, ill) -> id
rbindlist(id) -> id

#spatial df (coords+id+lon+lat)
coords = cbind(id$lon,id$lat)
sp_id <- SpatialPointsDataFrame(coords = coords, 
                               data = id,
                               proj4string = CRS("+proj=longlat"))

sp_id <- spTransform(sp_id, CRS("+init=epsg:2180"))

city_list <- list.dirs(recursive = F, full.names = FALSE) ## folder=city
data_airly<-airly_input$data

### tabela z wartosciami granicznymi, kolumna = miasto
df<-data.frame(KR = c(19.85,50.1,20.05,50),
               WAW = c(20.84,52.325,21.1,52.15)
               )
  
rownames(df) <- c("W","N","E","S")

data_fun <- function(city){
  subset(df, select = c(city)) ->df_fun
  W= df_fun[1,]
  N= df_fun[2,]
  E= df_fun[3,]
  S= df_fun[4,]
  subset(sp_id, lon>W & lon<E & lat>S & lat< N) -> data
  as.data.frame(data) -> get_id
  colnames(get_id)[1] <- "m_id"
  vals <-data_airly[data_airly$m_id %in% get_id$m_id,]
  saveRDS(vals, file = paste0(city, "/", city,"_val.RDS"))
  writeOGR(data, layer = 'sensor_data', paste0(city, "/pliki/", city,"_data.shp"), driver = "ESRI Shapefile")
  }

for(city in city_list){
  if (dir.exists(file.path(city, "pliki"))){
    data_fun(city)
  }
}
