#Make sure you have absorbance which is 2-log(T)
#Make sure time is in days (/24 for hours)
Time<-scan()
ABS<-scan()

readings<-data.frame(Time=Time, ABS=ABS)

readings$upper<-10^7


if(!require(devtools)){install.packages("devtools")}
if(!require(ggplot2)){install.packages("ggplot2")}
source_url("https://raw.github.com/edielivon/Useful-R-functions/master/Growth%20curves/logistic_growth_mle_norm.R")
source_url("https://raw.github.com/edielivon/Useful-R-functions/master/Growth%20curves/logistic_growth_nls.R")

# fit<-logistic.growth.mle.norm(readings=readings)
# 
# qplot(data=fit, x=Time, y=ABS)+
#   geom_line(aes(y=logistic.mle.predicted))
# 
# fit


fit<-logistic.growth.nls(readings=readings)

qplot(data=fit, x=Time, y=ABS)+
  geom_line(aes(y=logistic.nls.predicted))

fit

