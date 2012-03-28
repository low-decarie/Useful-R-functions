rm(list=ls())

setwd("~/Documents/PhD/CO2 calc/sub functions/general")
source("01_SetConstants.R")
source("02_default measure parameters.R")
source("03_CalculateInitialVolumes.R")
source("04_CalculateWaterVolumeAtTPot.R")
source("05_CalculateFinalVolumes.R")

setwd("~/Documents/PhD/CO2 calc/sub functions/CO2")
source("01_CalculateInitialMoleCO2.R")
source("02_equilibrateCO2.R")

CO2calc.fun<-function(Vbot20,
                      Vairi,
                      TempCWi,
                      TempCHi=TempCWi,
                      Pdbar,
                      TempCf,
                      Sal,
                      RHi=0,
                      alpha = 1e-05,
                      xCO2meas){


MassSW<-MassSW.fun(Vbot20, Vairi, alpha, TempCWi, Pdbar)

VwatPotT<-VwatPotT.fun(Vbot20, alpha, TempCWi, Pdbar, TempCpot)

Vwatf<-Vwatf.fun(Vbot20, alpha, TempCWi, Pdbar, TempCf)
Vbotf<-Vbotf.fun(Vbot20, alpha, TempCf)
Vairf<-Vairf.fun(Vbotf, Vwatf)

nCO2Hi<-nCO2Hi.fun(xCO2fill, TempCHi, Vairi, Sal, R = 82.05784, RHi)
                   
nCO2tot.fun<-nCO2tot.fun(nCO2Hi, MassSW, TCi = 0.0022)

nCO2Wf<-nCO2Wf(xCO2meas, MassSW, TempCf, Sal, Pmeas, RHf, nCO2Hi)

return(nCO2Wf)

}