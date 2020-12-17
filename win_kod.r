wd <- "/home/wpwp/test"
setwd(wd)
filenames<- (list.files(pattern='tape$',full=TRUE))

library(doParallel)

cl<-makeCluster(2)
registerDoParallel(cl,cores = 2)

ptime <- proc.time()
foreach(i=1:2) %dopar% {
  library(data.table)
  library(rtape)
  library(parsedate)
  library(data.table)
  library(parallel)
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
  loadAndRead<-function(file){
    rtapeLapply(file,flattenAirlyRecord) -> x
    sapply(x,function(x) is.null(x)||inherits(x,"try-error"))->bad
    rbindlist(x[!bad])->x 
    na.omit(x)->x
    unique(x)->x
    return(x)
  }
  input_fn=filenames[i]
  output_fn<-sprintf("%s.flattened.RDS",input_fn)
  if(!file.exists(output_fn)){
    loadAndRead(input_fn)->output
    saveRDS(output,file=output_fn)
  }
}

rbindlist(lapply(list.files(pattern='flattened.RDS$',full=TRUE),readRDS))->ans2
unique(ans)->ans2
proc.time()-ptime
