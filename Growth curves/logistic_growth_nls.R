logistic.growth.nls<-function(readings, upper=10, printer=T){
  
  if(printer){print(unique(readings$culture))}
  
  fitted.readings<-readings
  
  culture.model <- try(nls(formula=ABS ~ SSlogis(Time,Asym, xmid, scal),
                           data=readings,
                           na.action=na.exclude,
                           algorithm="port",
                           lower=c(0,0,0),
                           upper=c(upper,Inf, Inf)))
  
  if(class(culture.model)=="try-error"){
    
    fitted.readings$logistic.nls.xmid<-NA
    fitted.readings$logistic.nls.N0<-NA
    fitted.readings$logistic.nls.K<-NA
    fitted.readings$logistic.nls.r<-NA
    fitted.readings$logistic.nls.r.lower<-NA
    fitted.readings$logistic.nls.r.upper<-NA
    fitted.readings$logistic.nls.predicted<-NA
    
    
  }else{
    
    #extract the parameters from the model
    parameters<-t(data.frame(coef(culture.model,matrix=T)))
    #summary.culture.model<-coef(summary(culture.model))
    #print(summary.culture.model)
    
    #asign the value to each parameter
    Asym<-parameters[1]
    xmid<-parameters[2]
    scal<-parameters[3]
    
    #convert parameters to those used in ecological growth equations
    N0<-Asym/(1+exp(xmid/scal))
    K<-Asym
    r<-  1/scal
    
    interval<-confint(culture.model)
    
    r.lower<-1/interval["scal", "97.5%"]
    r.upper<-1/interval["scal", "2.5%"]
    
    fitted.readings$logistic.nls.xmid<-xmid
    fitted.readings$logistic.nls.N0<-N0
    
    fitted.readings$logistic.nls.K<-K
    
    fitted.readings$logistic.nls.r<-r
    fitted.readings$logistic.nls.r.lower<-r.lower
    fitted.readings$logistic.nls.r.upper<-r.upper
    
    fitted.readings$logistic.nls.predicted<-predict(culture.model)
    
    
  }
  
  return(fitted.readings)
}