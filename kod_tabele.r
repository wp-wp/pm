##BIBLIOTEKI I WORKING DIRECTORY##
library(rtape)
library(parsedate)
library(data.table)
wd <- "zmienna okreslajaca wd"
setwd(wd)

##FUNKCJE##
nullGoesNA<-function(x) if(is.null(x)) NA else x

getValue<-function(history,key){
  for(x in history$values) if(identical(x$name,key)) return(x$value)
  NA
}

getMeasurements<-function(history,key){
  if(is.null(history[[1]]$values)){
    #Old
    sapply(history,function(x) nullGoesNA(x[[c('measurements',tolower(key))]]))
  }else{
    #New
    sapply(history,getValue,toupper(key))
  }
}

flattenAirlyRecord<-function(x){
  if(is.null(x$ans)) return(NULL)
  try(data.frame(
    id=x$sensor$id,
    lat=x$sensor$location$latitude,
    lon=x$sensor$location$longitude,
    time=parse_iso_8601(sapply(x$ans$history,'[[','fromDateTime')),
    pm25=getMeasurements(x$ans$history,'pm25'),
    pm10=getMeasurements(x$ans$history,'pm10')
  ))
}
            ##rstudio##
loadAndRead<-function(file){
  rtapeLapply(file,flattenAirlyRecord) -> x
  sapply(x,function(x) is.null(x)||inherits(x,"try-error"))->bad
  rbindlist(x[!bad])->x 
  na.omit(x)->x
  unique(x)->x
  return(x)
}

            ##epyc##
loadAndReadParallel<-function(file){
  parLapply(rtapeAsList(file),flattenAirlyRecord,mc.cores=12)->x
  slapply(x,function(x) is.null(x)||inherits(x,"try-error"))->bad
  rbindlist(x[!bad])->x
  na.omit(x)->x
  unique(x)->x
  return(x)
}
           
##WCZYTYWANIE##           
for(input_fn in list.files(pattern='tape$',full=TRUE)){
  output_fn<-sprintf("%s.flattened.RDS",input_fn)
  if(!file.exists(output_fn)){
    loadAndReadParallel(input_fn)->output
    saveRDS(output,file=output_fn)
  }
}

