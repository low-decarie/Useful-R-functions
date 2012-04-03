exponential.growth.nls<-function(readings){
  
  fitted.readings<-readings
  
  ABS<-readings$ABS
  
  exp.model<-try(nls(formula =ABS ~ ABS0 * 2 ^ (growth.rate * Time),
                      start = list(ABS0 = 0.1, growth.rate = 1),
                      data=readings,
                      na.action=na.exclude))
  
  if(class(exp.model)=="try-error"){
    
    fitted.readings$N0<-NA
    fitted.readings$r<-NA
    fitted.readings$predicted<-NA
    
  }else{
    
    exp.parameters<-t(data.frame(coef(exp.model,matrix=T)))
    
    fitted.readings$N0<-exp.parameters[1]
    fitted.readings$r<-exp.parameters[2]
    fitted.readings$predicted<-predict(exp.model)
    
  }
  
  return(fitted.readings)
}