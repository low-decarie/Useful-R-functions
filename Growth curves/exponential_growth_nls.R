#Exponential fitting of growth data using nls with bounds and port algorythm



exponential.growth.nls<-function(readings){
  
  fitted.readings<-readings
  
  ABS<-readings$ABS
  
  exp.model<-try(nls(formula =ABS ~ ABS0 * 2 ^ (growth.rate * Time),
                      start = list(ABS0 = min(ABS,na.rm=T), growth.rate = 1),
                      data=readings,
                      na.action=na.exclude,
                     algorithm="port",
                     lower=c(0,0),
                     upper=Inf))
  
  if(class(exp.model)=="try-error"){
    
    fitted.readings$exp.N0<-NA
    fitted.readings$exp.r<-NA
    fitted.readings$exp.predicted<-NA
    
  }else{
    
    exp.parameters<-t(data.frame(coef(exp.model,matrix=T)))
    
    interval<-confint(exp.model)
    
    fitted.readings$exp.N0<-exp.parameters[1]
    fitted.readings$exp.r<-exp.parameters[2]
    fitted.readings$exp.r.lower<-interval["growth.rate", "2.5%"]
    fitted.readings$exp.r.upper<-interval["growth.rate", "97.5%"]
    
    fitted.readings$exp.predicted<-predict(exp.model)
    
  }
  
  return(fitted.readings)
}
