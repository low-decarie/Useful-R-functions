read.microplate<-function(file.name,
                          plate.width=12,
                          plate.height=8,
                          c.names=paste("C", 1:plate.width, sep="."),
                          r.names=LETTERS[1:plate.height],
                          col.id="columns",
                          row.id="rows",
                          measure="ABS",
                          frequency=1){
  
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
                    skip=14+(frequency-1)*11,
                    fill=T,
                    nrows=plate.height)
  
#Reformat data to more usable long format
  names(plate)<-c(row.id, c.names)
  
  plate[1]<-r.names
  
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