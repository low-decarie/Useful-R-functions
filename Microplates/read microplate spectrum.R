read.microplate.spectrum<-function(file.name,
                          plate.width=8,
                          plate.height=6,
                          start.freq=350,
                          end.freq=700,
                          steps.freq=10,
                          n.row=1,
                          n.steps=(end.freq-start.freq)/steps.freq,
                          c.names=paste(LETTERS[n.row], 1:plate.width, sep="."),
                          r.names=LETTERS[1:plate.height],
                          col.id="columns",
                          row.id="frequency",
                          measure="ABS"){
  
  #Run checks
  if(length(c.names)!=plate.width) print("Number of column names (c.names) does not match plate width")
  if(length(r.names)!=plate.height) print("Number of row names (r.names) does not match plate width")
  
  
  
  #Load data and extract time information
  time<-read.table(file.name,
                   header=F,
                   sep="=",
                   skip=4,
                   fill=T)
  time<-time[1,2]
  time<-strptime(time,
                 format="%m/%d/%Y %H:%M:%S")
  
  #Load data and read information
  plate<-read.table(file.name,
                    header=F,
                    sep=";",
                    skip=14+(n.row-1)*(n.steps+4),
                    fill=T,
                    nrows=n.steps,
                    na.strings="?????")
  
  #Reformat data to more usable long format
  names(plate)<-c(row.id, c.names)
  
  plate<-reshape(plate,
                 idvar=row.id,
                 ids= plate[1],
                 times=names(plate)[-1],
                 timevar=col.id,
                 direction="long",
                 varying=list(names(plate)[-1]))
  
  names(plate)[3]<-measure
  
  plate<-data.frame(plate, time=time)
  
  return(plate)
}


read.microplate.spectrum.list<-function(file.name,
                                        plate.height=6,
                                        ...){
  
  readings<-NULL
  for(row in 1:plate.height){
    print(row)
    try({
      readings.temp<-read.microplate.spectrum(file.name, n.row=row,...)
      readings.temp$row<-row
      readings<-rbind(readings, readings.temp)
    })
  }
  
  return(readings)
}

read.microplate.spectrum.list(file.name)
