#logistic.growth.mle.norm

logistic.growth.mle.norm<-function(readings, printer=F, upper=readings$upper){
  
  if(printer){print(unique(readings$culture))}
  
  fitted.readings<-readings
  
  #Log likelyhood function to be minimized
  like.growth<-function(parameters=c(1, 1, 0.01,0.1), readings){
    
    #Parameter extraction
    K<-parameters[1]
    r<-parameters[2]
    N0<-parameters[3]
    #alpha<-parameters[4]
    st.dev<-parameters[4]
    
    #Data extraction
    ABS<-readings$ABS
    Time<-readings$Time
    
    #Logistic growth model
    Nt<-(K*N0*exp(r*Time) ) / (K + N0 * (exp(r*Time)-1))
    #Nt<-(N0*K) / (N0 + (K-N0)*exp(-r*t))  #Synonymous model
    
    #log likelihood estimate
    #Nomral distribution
    likelihood<- -sum(dnorm(ABS, Nt, sd=st.dev, log=T))
    
    # Sanity bounds (remove if using "L-BFGS-B" or constrOptim
    if(any(c(Nt<0,
             Nt>upper, 
             K>upper,
             N0<0,
             r<0,
             st.dev<0,
             st.dev>upper))){likelihood<-NA}
    
    
    return(likelihood)
    
  }
  
  try.test<-try({
    
    fit<-optim(par=c(1, 1, 0.01,0.1),
                             fn=like.growth,
                      readings=readings)
   

#      fit<-optim(par=c(1, 1, 0.01,0.1),
#                                fn=like.growth,
#                                readings=readings,
#                                 method="L-BFGS-B",
#                                upper=c(50, 50, 50, 50),
#                                lower=c(0,0,0,0))
  
#        fit<-constrOptim(theta=c(1, 1, 0.01,0.1),
#                                  f=like.growth,
#                                  readings=readings,
#                         ui=??,
#                         ci=??)
#
#  
#  library(stat4)   
#  fit<-mle(start=c(1, 1, 0.01,0.1),
#              minuslogl=like.growth,
#              readings=readings)
  
  
  #extract fit values
  K<-fit$par[1]
  r<-fit$par[2]
  N0<-fit$par[3]
  Time<-readings$Time
  
  predicted<-(K*N0*exp(r*Time) ) / (K + N0 * (exp(r*Time)-1))
  
  fitted.readings$logistic.mle.N0<-N0
  fitted.readings$logistic.mle.K<-K
  fitted.readings$logistic.mle.r<-r
  fitted.readings$logistic.mle.predicted<-predicted
})
  
   #Pad with NAs for failed fits                                                             
   if(class(try.test)=="try-error"){
     fitted.readings$logistic.mle.N0<-NA
     fitted.readings$logistic.mle.K<-NA
     fitted.readings$logistic.mle.r<-NA
     fitted.readings$logistic.mle.predicted<-NA
   }
  
   return(fitted.readings)                                                     
                                                        
}
                                                            
                                        