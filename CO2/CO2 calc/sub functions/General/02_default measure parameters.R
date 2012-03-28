#Set default measurement parameters


#SetBottleAndFillParameters;
        Vbot20 = 60           # ml  ?Volume of bottle
        Vairi = 6             # ml
        TempCHi = 5           # deg C
        Ptoti = 1             # atm
        xCO2fill = .00036      # DRY-air mole fraction of CO2 in fill gas
        TempCWi = 5           # deg C
          
#SetSampleDefaults;
        Sal = 35              # mille
        TCi = .0022            # mol/kg-SW
        TempCpot = 5          # deg C
        O2conc = 308.1         # umol/kg-SW (100% at T = 5, S = 35, P = 0)
        O2satpct = 100        # 100% (at TempCpot)
        N2satpct = 100        # 100% (at TempCpot)
        Arsatpct = 100        # 100% (at TempCpot)
        TA = .0023             # mol/kg-SW
        RHi = 0
#
#SetEquilibrationDefaults;
        Pmeas = 1.1796         # atm (so that N2 sat pct comes out 100%)
        xCO2meas = .001        # ppm
        TempCf = 25           # deg C
        Vdeadvol = .3          # ml
        Pdeadvol = 1          # atm