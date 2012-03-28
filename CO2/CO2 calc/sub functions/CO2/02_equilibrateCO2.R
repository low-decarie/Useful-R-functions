#EquilibrateCO2Here:
#       This is after equilibration


nCO2Wf.fun<-function(xCO2meas,
                    MassSW,
                    TempCf = 25,
                    Sal=35,
                    Pmeas=1,
                    RHf = 0,
                     nCO2Hi,
                     TCi){
  
  #Load or install libraries
  if (!require(seacarb)) install.packages("seacarb")
 
          Ptotf=Pmeas #we assume that equilibration and measurement pressures are measured atmospheric pressure
          Pwv <- vapor(S=Sal,
                     t=TempCHi)
  
          
  
          pH2Of = Pwv * RHf / 100
  
  
          TempKf = TempCf + 273.15
  
  
          nCO2Hf = xCO2meas * (Ptotf - pH2Of) * Vairf / (R * TempKf)
  
          TCf = TCi - (nCO2Hf - nCO2Hi) / MassSW
  
          nCO2Wf = TCf * MassSW
  
          attr(nCO2Wf,"unit")<-"moles"
  
  
        return(nCO2Wf)
  
  }