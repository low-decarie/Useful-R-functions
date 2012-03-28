logistic.growth.mle<-function(readings){
  
  print(unique(readings$culture))
  
  fitted.readings<-readings
  
  #Log likelyhood function to be minimized
  like.growth<-function(parameters=c(0.3, 1, 0.1,1), t, Nt){
    
    #Parameter extraction
    K<-parameters[1]
    r<-parameters[2]
    N0<-parameters[3]
    sd<-parameters[4]
    
    #Logistic growth model
    growth<-(K*N0*exp(r*t) ) / (K + N0 * (exp(r*t)-1))
    #growth<-(N0*K) / (N0 + (K-N0)*exp(-r*t))  #Synonymous model
    
    #log likelihood estimate
    #nomral distribution
    
    likelihood<- -sum(dnorm(x=Nt, mean=growth, sd=sd, log=T))
    
    #Bounds
    if(K > 0.9) likelihood <- 9 * 10^10
    if(K < 0) likelihood <- 9 * 10^10
    
    if(r > 3) likelihood <- 9 * 10^10
    if(K < 0) likelihood <- 9 * 10^10
    
    if(N0 > 0.2) likelihood <- 9 * 10^10
    if(N0 < 0) likelihood <- 9 * 10^10
    
    
    #logistic distribution
    #likelihood<- -sum(dlogis(x=Nt, location=growth, scale=1, log=T))
    
    #exponential  (obviously not!!!)
    #likelihood<- -sum(dexp(x=Nt, rate=growth, log=T))
    
    #weibull
    #likelihood<- -sum(dweibull(x=Nt, shape=3, scale=, log=T))
    
    return(likelihood)
    
  }
  
  fit<-with(readings, optim(par=c(0.3, 1, 0.1,1),
                            fn=like.growth,
                            t=Time,
                            Nt=ABS))
  
  #  parnames(like.growth)<-c("K", "r", "N0")
  #  fit<-with(readings, mle2(start=c(K=1, r=1, N0=0.1),
  #                             minuslogl=like.growth,
  #                             data=list(t=Time, Nt=ABS)))
  
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