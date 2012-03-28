#Setting constants
        
        R = 82.05784  #The gas constant (R) in cm^3 atm K^−1 mol^−1
                        #or library(marelac); R=100*Constants$gasCt1
        
        alpha = .00001  #volume expansion coefficient for borosilicate glass
                        #for dry air (this assumes xCO2 is .00036)
        
        xN2atm = .78084
        xO2atm = .20946
        xAratm = .00934
        
        TP = 0  #no nutrients included
        TSi = 0  #no nutrients included
        
        T_4 = TP
        T_5 = TSi
        
        Pref = 0  #reference pressure for potential temperature