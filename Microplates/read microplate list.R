read.microplate.list<-function(file.list,...){
  
  readings<-NULL
  for(file in file.list){
    print(file)
    try({
        readings.temp<-read.microplate(file,...)
        readings.temp$file<-file
        readings<-rbind(readings, readings.temp)
        })
  }
  
  return(readings)
}