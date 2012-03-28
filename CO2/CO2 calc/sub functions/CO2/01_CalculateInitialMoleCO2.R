#CalculateInitialMolesCO2:


#       For a CO2-air mixture nMoles = Ptoti * Vairi / (R * TempKi) since
  #               the non-ideality of the mixture is small.
  #       xCO2fill is a MOLE ratio (in DRY air), so the fugacity correction
  #       is so small it is not needed (it is proportional to xCO2fill)


 nCO2Hi.fun<-function(xCO2fill = .00036,
                       TempCHi=5,
                       Vairi=6,
                       Sal = 35,
                       R = 82.05784,
                       RHi = 0){
  
        #Load or install libraries
        if (!require(marelac)) install.packages("marelac")
  
        TempKi = TempCHi + 273.15
        
        Pwv <- vapor(S=Sal,
                     t=TempCHi)
                     
        
        pH2Oi = Pwv * RHi / 100
        
        Fac1 = (Ptoti - pH2Oi) * Vairi / (R * TempKi)
        
        nCO2Hi = xCO2fill * Fac1
        
        attr(nCO2Hi, "unit")<- "ppm"
        
        return(nCO2Hi)
    
 }
 
 
 nCO2Hi<- nCO2Hi.fun()
       
nCO2tot.fun <- function(nCO2Hi,
                   MassSW,
                   TCi = .0022){
        
        nCO2Wi = TCi * MassSW
        
        nCO2tot = nCO2Hi + nCO2Wi

        return(nCO2tot)
        
  }


nCO2tot<- nCO2tot.fun(nCO2Hi,MassSW)