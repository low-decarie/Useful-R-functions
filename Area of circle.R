#Area of a circle

circ.area<-function(diameter){
  area<-pi*(diameter/2)^2
  return(area)
  }

circ.dia<-function(area){
  diameter<-2*sqrt(area/pi)
  return(diameter)
}

half.area<-function(diameter){
  large<-circ.area(diameter)
  small<-large/2
  half.dia<-sqrt(small/pi)*2
  return(half.dia)
  }


diameters<-data.frame(large=c(0.010,
                              0.015,
                              0.020,
                              0.030,
                              0.035,
                              0.040,
                              0.045,
                              0.051,
                              0.056,
                              0.060,
                              0.065,
                              0.073,
                              0.081,
                              0.090,
                              0.100,
                              0.110,
                              0.125))


diameters$small<-half.area(diameters$large)

half.dia.coef<-coef(lm(diameters$small~diameters$large))[2]

plot(diameters$small~diameters$large)

