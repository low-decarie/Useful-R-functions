K<-0.8
r<-1
N0<-0.01
t<-1:10
max<-1

Nt<-(K*N0*exp(r*t) ) / (K + N0 * (exp(r*t)-1))

demo<-qplot(x=t,
      y=Nt,
      geom="line",
      ylim=c(0,max))+
      geom_hline(yintercept=I(1))


a<-seq(0,1,0.01)
b<-dlogitnorm(q=x, mu=0.8, sigma=0.8)
#plot(y~x, type="l")

demo<-demo+geom_polygon(aes(x=9-b/4, y=a), alpha=I(0.2))

c<-seq(0,1,0.01)
d<-dlogitnorm(q=x, mu=0, sigma=0.8)
plot(y~x, type="l")

demo<-demo+geom_polygon(aes(x=5-d/4, y=c), alpha=I(0.2))


e<-seq(0,1,0.01)
f<-dlogitnorm(q=x, mu=-2, sigma=0.8)
plot(y~x, type="l")

demo<-demo+geom_polygon(aes(x=2-f/4, y=e), alpha=I(0.2))



print(demo)