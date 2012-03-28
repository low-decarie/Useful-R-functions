#Calculate initial mass of seawater

#Translation of the BASIC script HEADSPAC written by Ernie Lewis
#Uses functions found in package 'seacarb'

#Load or install libraries
if (!require(seacarb)) install.packages("seacarb")

MassSW.fun <- function(Vbot20 = 60,
                       Vairi=6,
                         alpha = 1e-05, 
                         TempCWi = 5,
                         Pdbar = 0) {
  
    Vboti = Vbot20 * (1 + alpha * (TempCWi - 20))
    
    Vwati = Vboti - Vairi
    
    DSW <- rho(S = Sal,
               T = TempCWi,
               P = Pdbar)
    
    MassSW = Vwati * DSW
    
    attr(MassSW, "unit") <- "(kg)"
    
    return(MassSW)
}

MassSW = MassSW.fun()