#Calculate Water Volume At Potential Temperature:
#       This is needed for calculating percent saturations, which are
#               referenced to TempCpot, which may be different from TempCWi
#               (though it is assumed that even if the water warms up before
#               filling, no gas exchange occurs).


VwatPotT.fun<-function(Vbot20 = 60,
                    alpha = 1e-05,
                    TempCWi = 5,
                    Pdbar = 0,
                    TempCpot = 5){
  
  MassSW<-MassSW.fun(Vbot20,
                     alpha,
                     TempCWi,
                     Pdbar)
  
  DSW <- rho(S = Sal,
           T = TempCpot,
           P = Pdbar)
  
  VwatPotT = MassSW / DSW
  
  attr(VwatPotT, "unit")<- "mL"
 
  return(VwatPotT)
}

VwatPotT<-VwatPotT.fun()
  