library(jpeg)
library(plyr)

#Load all jpeg files
file.list<-list.files("~/Desktop/Colony Phenotype Dark GB Aug2010/")
file.list<-file.list[grep(".jpg", file.list)]

#test
#file.list<-file.list[1]

#Open pdf
pdf("~/Desktop/Colony Phenotype Dark GB Aug2010/all_combined_and_labeled.pdf")

#Read and plot each file using file name a main title
l_ply(file.list,
      .progress="text",
      function(i){
  path<-paste("~/Desktop/Colony Phenotype Dark GB Aug2010/",i,sep="")

#load the file
  image.file<- readJPEG(path)
  
#creat a blank plot with the aspect ratio of the original file
plot(1:2,1:2, asp=1536/2080,
     bty='n',
     type='n',
     xaxt='n',
     yaxt='n',
     xlab="",
     ylab="",
     main=i)
rasterImage(image.file, 1, 1, 2, 2)
})

graphics.off()