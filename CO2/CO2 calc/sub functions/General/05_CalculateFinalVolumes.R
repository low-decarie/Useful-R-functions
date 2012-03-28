#CalculateFinalVolumes:
#       This is after equilibration.
        
Vwatf.fun<- function(Vbot20 = 60,
                    alpha = 1e-05,
                    TempCWi = 5,
                    Pdbar = 0,
                    TempCf = 5){
      
        MassSW<-MassSW.fun(Vbot20,
                     alpha,
                     TempCWi,
                     Pdbar)
  
        DSW <- rho(S = Sal,
                 T = TempCpot,
                 P = TempCf)
        
        Vwatf = MassSW / DSW
        
        attr(Vwatf, "unit") <- "mL"
        
        return(Vwatf)
      
    }


Vwatf<- Vwatf.fun()


Vbotf.fun<-function(Vbot20 = 60,
                    alpha = 1e-05,
                    TempCf = 5){

        Vbotf = Vbot20 * (1 + alpha * (TempCf - 20))
        
        return(Vbotf)
  }


Vbotf <- Vbotf.fun()

        
Vairf.fun<-function(Vbotf, Vwatf){
  Vairf = Vbotf - Vwatf}