#Logistic growth fitting using nls and bounds with port

#Required improvements
  #SSlogis function with integrated bounds
  #Confidence intervals of parameters using confint)

logistic.growth.nls<-function(readings, upper=10, printer=T){
  
  if(printer){print(unique(readings$culture))}
  
  fitted.readings<-readings
  
  
  #lower.v=c(0,0,0)
  #upper.v=c(upper,Inf, Inf)
  #start.values<-getInitial(ABS~SSlogis(Time,Asym, xmid, scal), data=readings)
  #start.values[start.values<=lower.v]<-10^-5
  #start.values[start.values>=upper.v]<-upper.v[start.values>=upper.v]-10^-5
  #
  #Initially used ABS~SSlogis, but it would often start out of bounds
  
  start.values=c("K"=1, "r"=1, "N0"=0.01)
  
  
  culture.model <- try(nls(formula=ABS ~(K*N0*exp(r*Time) ) / (K + N0 * (exp(r*Time)-1)),
                           data=readings,
                           start=start.values,
                           na.action=na.exclude,
                           algorithm="port",
                           lower=c(0,0,0),
                           upper=c(upper,Inf, Inf)))
  
  if(class(culture.model)=="try-error"){
    
    fitted.readings$logistic.nls.N0<-NA
    fitted.readings$logistic.nls.K<-NA
    fitted.readings$logistic.nls.r<-NA
    #fitted.readings$logistic.nls.r.lower<-NA
    #fitted.readings$logistic.nls.r.upper<-NA
    fitted.readings$logistic.nls.predicted<-NA
    
    
  }else{
    
    #extract the parameters from the model
    parameters<-coef(culture.model)
    #summary.culture.model<-coef(summary(culture.model))
    #print(summary.culture.model)
    
    
    #This was for use with SSlogis
    #asign the value to each parameter
#     Asym<-parameters[1]
#     xmid<-parameters[2]
#     scal<-parameters[3]
    
    #convert parameters to those used in ecological growth equations
#     N0<-Asym/(1+exp(xmid/scal))
#     K<-Asym
#     r<-  1/scal
    
    #interval<-confint(culture.model)
    
    #r.lower<-1/interval["scal", "97.5%"]
    #r.upper<-1/interval["scal", "2.5%"]
    

    fitted.readings$logistic.nls.N0<-parameters["N0"]
    
    fitted.readings$logistic.nls.K<-parameters["K"]
    
    fitted.readings$logistic.nls.r<-parameters["r"]
    #fitted.readings$logistic.nls.r.lower<-r.lower
    #fitted.readings$logistic.nls.r.upper<-r.upper
    
    fitted.readings$logistic.nls.predicted<-predict(culture.model)
    
    
  }
  
  return(fitted.readings)
}