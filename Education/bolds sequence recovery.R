rm(list=ls())

# library(plyr)
# library(doMC)
# registerDoMC(2)


setwd("~/Documents/PhD/TA/BIO112 TPULSE/sequences")

#Three bolds sequences
#test.file<-"http://services.boldsystems.org/eFetch.php?record_type=full&id_type=sampleid&ids=(DQ116162,DHJPAR0020258,moth348.01)&return_type=text"
#The complete bolds data base
#complete<-"http://services.boldsystems.org/eFetch.php?record_type=full&id_type=sampleid&ids=(*)&return_type=text"
#Complete from file
complete<-"/Users/LowDecarie/Documents/PhD/TA/BIO112\ TPULSE/sequences/full.txt"

#sequences<-read.delim(file=complete, skip=0, nrows=1, header=F)
sequences.id<-data.frame("file.name", "recordID", "genus_name", "species_name")
write.table(x=sequences.id, file="sequences_id.csv", append=F, sep = ",", row.names=F, col.names=F)

k<-1
i<-1
while(k < 1000){
  
  sequences<-read.delim(file=complete, skip=i, nrows=1, header=F)
  sequence.compare<-read.csv(file="sequences_id.csv", skip=k-1, nrows=1, header=F)
  
  if(! is.na(sequences$V24)){
    if(as.character(sequences$V24)!=as.character(sequence.compare$V4)){
        writeLines(text=as.character(sequences$V55), con=paste(k, ".txt", sep=""))
        sequences.id<-c(k, sequences[,c("V3","V22", "V24")])
        write.table(x=sequences.id, file="sequences_id.csv", append=T, sep = ",", row.names=F, col.names=F)
        print("kept")
        k<-k+1
    }
  }
  i<-i+1
  print(paste(k,"/", i))
}