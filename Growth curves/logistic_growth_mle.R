library(devtools)
source_url("https://raw.github.com/edielivon/Useful-R-functions/master/Growth%20curves/logitnorm.R")

logistic.growth.mle<-function(readings){
  
  print(unique(readings$culture))
  
  fitted.readings<-readings
  
  #Log likelyhood function to be minimized
  like.growth<-function(parameters=c(1, 1, 0.01,2), t, Nt, ABS){
    
    #Parameter extraction
    K<-parameters[1]
    r<-parameters[2]
    N0<-parameters[3]
    #alpha<-parameters[4]
    beta<-parameters[4]
    
    #Logistic growth model
    Nt<-(K*N0*exp(r*t) ) / (K + N0 * (exp(r*t)-1))
    #growth<-(N0*K) / (N0 + (K-N0)*exp(-r*t))  #Synonymous model
    
    #log likelihood estimate
    #nomral distribution

    #does not work
    #likelihood<- -sum(dlogitnorm(q=(ABS-N0)/(K-N0), mu=((Nt-N0)-(K-N0)/2), sigma=beta, log=T))
    
    #likelihood<- -sum(dnorm(x=logit((ABS-N0)/(K-N0)), mean=(Nt-N0)-(K-N0)/2, sd=beta, log=T))
    
    likelihood<- -sum(dnorm(ABS, Nt, sd=beta, log=T))
    
    return(likelihood)
    
  }
  
  fit<-with(readings, optim(par=c(1, 1, 0.01,2),
                            fn=like.growth,
                            t=Time,
                            ABS=ABS))
  
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