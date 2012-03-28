read.tps = function(data) {
  a = readLines(data)
  LM = grep("LM", a)
  CURVES = grep("CURVES", a)
  POINTS = grep("POINTS", a)
  ID.ind = grep("ID", a)
  SCALE.ind = grep("SCALE", a)
  
  skip = LM
  
  l = length(LM)
  landmarks.temp = vector("list", l)
  
  for (i in 1:l) {
    LM.l=read.table(file=data, header=F, 
                          skip=skip[i]-1, nrows=1, sep="=")[,2]
    
    IMAGE.temp=read.table(file=data, header=F, 
                    skip=ID.ind[i]-2, nrows=1, sep="/", col.names="IMAGE")[,1]
    
    print(as.character(IMAGE.temp[length(IMAGE.temp)]))
            
    landmarks.temp[i] = list(data.frame(
      read.table(file=data, header=F, skip=LM[i], 
                 nrows=LM.l, col.names=c("X", "Y")),
      LM.num=1:LM.l,
      IMAGE = as.character(IMAGE.temp[length(IMAGE.temp)]),
      ID=read.table(file=data, header=F, 
                            skip=ID.ind[i]-1, nrows=1, sep="=", col.names="ID")[2,],
      Scale = read.table(file=data, header=F, 
                         skip=SCALE.ind[i]-1, nrows=1, sep="=")[,2]))
  }
  landmarks<-do.call(rbind, landmarks.temp)
  
  return(landmarks)
}