rm(list=ls())

source("~/Documents/Useful-R-functions/Growth\ curves/logitnorm.R")


par(mfrow=c(5,5))

x<-seq(0,1,0.01)

for(mu in seq(-2,2, 0.5)){
  for(sigma in 0:3){
    print(paste("mu=", mu, "sigma=", sigma))
    try(plot(dlogitnorm(x, mu=mu, sigma=sigma)~x,
             main=paste("mu=", mu, "sigma=", sigma),
             ylab="density"))
  }
}



##############################

rm(list=ls())


logit <- plogis

dlogitnorm <- function(x, ..., a = 0, b = 1, log = FALSE)
{
  out <- dnorm(x = logit(x, a, b), ..., log = TRUE) + log(b - a) - log(x - a) - log(b - x)
  out[x == a | x == b] <- -Inf
  if(log) out else exp(out)
}



x<-seq(0,1,0.01)

par(mfrow=c(6,6))


for(mu in seq(-2,2, 0.5)){
  for(sigma in 1:3){
    print(paste("mu=", mu, "sigma=", sigma))
    try(plot(dlogitnorm(x, a=mu, b=sigma)~x,
             main=paste("mu=", mu, "sigma=", sigma),
             ylab="density"))
  }
}
