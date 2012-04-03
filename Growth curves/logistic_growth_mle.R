source("~/Documents/Useful-R-functions/Growth\ curves/logitnorm.R")

logistic.growth.mle<-function(readings){
  
  print(unique(readings$culture))
  
  fitted.readings<-readings
  
  #Log likelyhood function to be minimized
  like.growth<-function(parameters=c(0.3, 1, 0.01,1,1), t, Nt){
    
    #Parameter extraction
    K<-parameters[1]
    r<-parameters[2]
    N0<-parameters[3]
    alpha<-parameters[4]
    beta<-parameters[5]
    
    #Logistic growth model
    Nt<-(K*N0*exp(r*t) ) / (K + N0 * (exp(r*t)-1))
    #growth<-(N0*K) / (N0 + (K-N0)*exp(-r*t))  #Synonymous model
    
    #log likelihood estimate
    #nomral distribution

    #does not work
#   likelihood<- -sum(dlogitnorm(q=Nt/K, mu=Nt-K/2, sigma=beta, log=T))
    
    likelihood<- -sum(dnorm(x=logit(Nt/K), mean=alpha, sd=beta, log=T))
    
    return(likelihood)
    
  }
  
  fit<-with(readings, optim(par=c(0.3, 1, 0.01,1,1),
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