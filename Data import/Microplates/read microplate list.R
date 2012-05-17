read.microplate.list<-function(file.list="", directory=".", ext="TXT",...){

  
  if(file.list==""){
  file.list<-paste(directory,"/", list.files(path=directory, pattern=ext), sep="")
  }
  
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
