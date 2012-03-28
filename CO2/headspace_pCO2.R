headCO2<-function(startCO2=0,
                  raw.pCO2,
                  initial.temp,
                  final.temp,
                  pressure=101.325,
                  head.ratio=1,
                  salinity){
  
	mol.vol<-(0.082057*(initial.temp +273.15))*(101.325/pressure)
	      
  pKH.initial.temp<-10^-(-((9345.17/(initial.temp+273.15))-
          60.2409+23.3585*log((273.15+initial.temp)/100)+
          salinity*(0.023517-0.023656*((273.15+initial.temp)/100)+
          0.0047036*((273.15+initial.temp)/100)^2))/log(10))
 
 pKH.final.temp<-10^-(-((9345.17/(final.temp+273.15))-
                60.2409+23.3585*log((273.15+final.temp)/100)+
                salinity*(0.023517-0.023656*((273.15+final.temp)/100)+
                0.0047036*((273.15+final.temp)/100)^2))/log(10))
	
	pCO2<-((raw.pCO2-startCO2)* head.ratio/(mol.vol)+(raw.pCO2* pKH.initial.temp))/pKH.final.temp

	return(pCO2)
	}

# #Example
# #Freshwater
# start<-c(5,5,380,380,1500)
# raw<-c(190, 220, 349, 355, 930)
# CO2.calc<-headCO2(startCO2=start,
#                   raw.pCO2=raw,
#                   initial.temp=29.8,
#                   final.temp=30.2,
#                   pressure=101.325,
#                   head.ratio=1,
#                   salinity=0.4)
# plot(CO2.calc~start)
# summary(lm(CO2.calc~start))
# 
# #seawater
# start<-c(5,5,380,378,1040, 1200)
# raw<-c(296, 277, 401, 401, 606, 630)
# CO2.calc<-headCO2(startCO2=start,
#                   raw.pCO2=raw,
#                   initial.temp=30.2,
#                   final.temp=30.4,
#                   pressure=101.325,
#                   head.ratio=1,
#                   salinity=33.2)
# plot(CO2.calc~start)
# summary(lm(CO2.calc~start))

