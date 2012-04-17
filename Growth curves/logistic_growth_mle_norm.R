logistic.growth.mle<-function(readings, printer=F){
  
  if(printer){print(unique(readings$culture))}
  
  fitted.readings<-readings
  
  #Log likelyhood function to be minimized
  like.growth<-function(parameters=c(2, 1, 0.01,2), t, Nt, ABS){
    
    #Parameter extraction
    K<-parameters[1]
    r<-parameters[2]
    N0<-parameters[3]
    #alpha<-parameters[4]
    sd<-parameters[4]
    
    #Logistic growth model
    Nt<-(K*N0*exp(r*t) ) / (K + N0 * (exp(r*t)-1))
    #Nt<-(N0*K) / (N0 + (K-N0)*exp(-r*t))  #Synonymous model
    
    #log likelihood estimate
    #nomral distribution
    likelihood<- -sum(dnorm(ABS, Nt, sd=sd, log=T))
    
    return(likelihood)
    
  }
  
  fit<-with(readings, optim(par=c(2, 1, 10^-10,2),
                            fn=like.growth,
                            t=Time,
                            ABS=ABS))
  

  
  #  print(fit$par)
  K<-fit$par[1]
  r<-fit$par[2]
  N0<-fit$par[3]
  t<-readings$Time
  
  predicted<-(K*N0*exp(r*t) ) / (K + N0 * (exp(r*t)-1))
  
  fitted.readings$No<-N0
  fitted.readings$K<-K
  fitted.readings$r<-r
  fitted.readings$predicted<-predicted
   
  return(fitted.readings)
}