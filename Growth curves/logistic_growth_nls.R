logistic.growth.nls<-function(readings){
  
  fitted.readings<-readings
  
  culture.model <- try(nls(formula=ABS ~ SSlogis(Time,Asym, xmid, scal),
                           data=readings,
                           na.action=na.exclude))
  
  if(class(culture.model)=="try-error"){
    
    fitted.readings$xmid<-NA
    fitted.readings$No<-NA
    fitted.readings$K<-NA
    fitted.readings$r<-NA
    fitted.readings$predicted<-NA
    
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
    No<-Asym/(1+exp(xmid/scal))
    K<-Asym
    r<-  1/scal
    
    fitted.readings$xmid<-xmid
    fitted.readings$No<-No
    fitted.readings$K<-K
    fitted.readings$r<-r
    fitted.readings$predicted<-predict(culture.model)
    
    
  }
  
  return(fitted.readings)
}