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
  mclapply(rtapeAsList(file),flattenAirlyRecord,mc.cores=48)->x
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

##WCZYTYWANIE2##
ptm <- proc.time()
for(input_fn in list.files(pattern='tape$',full=TRUE)){
  output_fn<-sprintf("%s.flattened.RDS",input_fn)
  if(!file.exists(output_fn)){
    saveRDS(loadAndReadParallel(input_fn),file=output_fn)         ##<--- bez pliku output##
  }
}
proc.time()-ptm

          
          ##inne próby użyte z pętlą for##   ##obie wersje mają podobne czasy do zwykłej pętli for - funkcja loop jest minimalnie szybsza##
loop<- function(x){                                       
  output_fn<-sprintf("%s.flattened.RDS",x)
  if(!file.exists(output_fn)){                            ##<--- zastanawialam sie czy nie zrobic funkcji w tym rodzaju i nie uzyc lapply, ale nie jestem pewna czy lapply nie 
    saveRDS(loadAndReadParallel(input_fn),file=output_fn)         ##     zepsuloby plikow wynikowych##
  }
}

##lub Windows##          
oneFileLoop <-  function(x) {
  output<- sprintf("%s.21flattened.RDS",x)
  if(!file.exists(output_fn)){
    rtapeLapply(x,flattenAirlyRecord) -> x
    sapply(x,function(x) is.null(x)||inherits(x,"try-error"))->bad
    rbindlist(x[!bad])->x 
    na.omit(x)->x
    unique(x)->x
    saveRDS(x,file=output)
    }
}
