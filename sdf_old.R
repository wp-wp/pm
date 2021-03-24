library(rgdal)
library(geometry) 

setwd("...")

##sensory & id
points <- readOGR("KR_data.shp") 
points <- spTransform(points, CRS("+init=epsg:4258"))
points<- data.frame(lon=c(points$lon),lat=c(points$lat))
m_id <- readRDS("id_KR.RDS")

if(class(m_id)=="data.frame") len = nrow(m_id) else len= length(m_id)

##funkcje
vecLenght <- function(x) sqrt(sum(x^2))
sdf<- function(a,b,p){
  h = min(1, max(0, dot(p-a,b-a)/dot(b-a,b-a)))
  signed_field_distance = vecLenght(p-a-h*(b-a))
  return(signed_field_distance)
}  
getP <- function(y,j){
  p = as.vector(y[j,], mode = "numeric")
  return(p)
}
getA <- function(y,n){
  a = as.vector(y[n,], mode = "numeric")
  return(a)
}
getB <- function(y,m){
  b = as.vector(y[m,], mode = "numeric")
  return(b)
}
sdfFun <- function(x,y){
  p_output <- list()
  sdf_output <- list()
  for (i in 1:nrow(x)) {
    for (n in 1:(nrow(y)-1)) {
      p <- getA(x,i)
      a <- getA(y,n)
      b <- getB(y,n+1)
      sdf_output[[n]] <- sdf(a,b,p)
    }
    p_output[[i]]<-min(unlist(sdf_output))
  }
  return(p_output)
}
all <- function(sensor, road, m_id){
  lapply(lines2, sdfFun, x=points)->sdf_lines
  
  df<- matrix(ncol = len, nrow = length(sdf_lines))
  
  for (k in 1:len) {
    for (l in 1:length(sdf_lines)) {
      df[l,k]<- as.numeric(paste(sdf_lines[[l]][k]))
    }
  }
  data.frame(sdf = apply(df,2,min))->sdf_min
  cbind(m_id,sdf_min)->sdf_min
  return(sdf_min)
}

setwd("...")

start_time <- Sys.time()
for(input_fn in list.files(pattern='shp$')){
  output_fn<-sprintf("%s.sdf.RDS",input_fn)
  lines <- readOGR(input_fn) 
  lines <- spTransform(lines, CRS("+init=epsg:4258"))
  lines2<- coordinates(lines)
  lapply(lines2,as.data.frame)->lines2
  if(!file.exists(output_fn)){
    all(points,lines2,m_id)->output
    saveRDS(output,file=output_fn)
  }
}
end_time <- Sys.time()
end_time - start_time
