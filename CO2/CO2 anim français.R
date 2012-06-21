rm(list=ls())

library(animation)
library(plotrix)
library(ggplot2)

CO2<-read.table(file="ftp://ftp.cmdl.noaa.gov/ccg/co2/trends/co2_mm_gl.txt",
                col.names=c("year", "month", "decimal", "average", "trend"))


historic<-read.fwf(file="http://cdiac.ornl.gov/ftp/trends/co2/lawdome.combined.dat",
                   skip=22,
                   widths=rep(13, times=7),
                   as.is=c(T,T,T,T,T,T,T))

historic<-data.frame(year=as.numeric(historic[1:31,5]),
                     average=as.numeric(historic[1:31,7]))

historic$type<-"Mesures à partir d'échantillons de glace"

CO2<-aggregate(x=list(average=CO2$average), by=list(year=CO2$year), FUN=mean)
CO2$type<-"Mesures atmosphériques"

future<-seq(max(CO2$year), 2100)

low<-log(700/max(CO2$average))/(2100-max(CO2$year))
high<-log(1000/max(CO2$average))/(2100-max(CO2$year))

future.low<-data.frame(year=future,
                       average=max(CO2$average)*exp(low*(future-max(CO2$year))),
                      type="Prédiction minimale")
                       
future.high<-data.frame(year=future,
                       average=max(CO2$average)*exp(high*(future-max(CO2$year))),
                      type="Prédiction maximale")


CO2<-rbind(CO2, future.low, future.high, historic)



CO2<-CO2[order(CO2$year),]



CO2.anim<-function(){
  
    for(i in unique(CO2$year)){
      selected<-CO2[CO2$year<=i,]
      plot.CO2<-qplot(y=average,
                          x=year,
                          data=selected,
                          geom="line",
                          size=I(5),
                          xlab="Temps (Années)",
                          ylab=expression("Concentration atmosphérique de CO"[2](ppm)),
                          colour=type)
      plot.CO2<-plot.CO2 + scale_colour_manual(values = c("Mesures à partir d'échantillons de glace"="blue",
                                                         "Mesures atmosphériques"="black",
                                                         "Prédiction minimale"="orange",
                                                          "Prédiction maximale"="red"))
      
      plot.CO2<-plot.CO2+theme_gray(35)
      
      plot.CO2<-plot.CO2 + opts(legend.position = "bottom")
      
      
      
      if(i <= 2011){plot.CO2 <- plot.CO2+
                                scale_y_continuous(limits=c(275, 390))+
                                scale_x_continuous(limits=c(1850, 2011))}

    	print(plot.CO2)
      
		}
		}




dev.off()
setwd("~/Documents/Useful-R-functions/CO2")
ani.options(ani.height = 1080, ani.width = 1920, interval = 0.05, nmax = length(unique(CO2$year)))
# saveMovie(CO2.anim(),
#          interval=0.1,
#          outdir="/Users/LowDecarie/Documents/PhD/Reports/Conferences/CSEE/CSEE 2010/global co2 concentration/movie images",
#          clean=F)


#
saveVideo(CO2.anim(),
        video.name="CO2animation.mp4",
         clean=T,
          other.opts = "-b 10000k")
