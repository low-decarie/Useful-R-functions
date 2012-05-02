chemtax<-function(sample.conc, pigment.matrix, baysian=F){
  

  sample.a<-data.matrix(sample.conc[,-1])
  dimnames(sample.a)[[1]]<-sample.conc[,1]
  
  pigment.a<-data.matrix(pigment.matrix[,-1])
  dimnames(pigment.a)[[1]]<-pigment.matrix[,1]
  
  #match names
  sample.a<-sample.a[ ,dimnames(sample.a)[[2]] %in% dimnames(pigment.a)[[2]]]
  pigment.a<-pigment.a[ ,dimnames(pigment.a)[[2]] %in% dimnames(sample.a)[[2]]]
  sample.a<-sample.a[,match(dimnames(pigment.a)[[2]],dimnames(sample.a)[[2]])]
  
  #Check match
  print(match(dimnames(pigment.a)[[2]],dimnames(sample.a)[[2]]))
  
  #Remove unidentifiable groups
  pigment.a<-pigment.a[rowSums(pigment.a)!=0,]
  

  pdf("./Plots/baysian baysian.pdf")
  
  X<-apply(X=sample.a, MARGIN=1,FUN=function(sample.a){
  #From Chemtax in limsolve
  # 2. Estimating the algal composition of the field sample.a
  Nx     <-nrow(pigment.a)
  
  # equations that have to be met exactly Ex=f: 
  # sum of all fraction must be equal to 1.
  EE <- rep(1,Nx)
  FF <- 1
  
  # inequalities, Gx>=h:
  # all fractions must be positive numbers
  GG <- diag(nrow=Nx)
  HH <- rep(0,Nx)
  
  # equations that must be reproduced as close as possible, Ax ~ b
  # = the field data; the input ratio matrix and field data are rescaled
  AA     <- pigment.a/rowSums(pigment.a)
  BB     <- sample.a/sum(sample.a)
  
  # 1. Solve with lsei method
  X <-lsei(t(AA),BB,EE,FF,GG,HH)$X
  

#   # 2. Bayesian baysian; 
#   # The standard deviation on the field data is assumed to be 0.01
#   # jump length not too large or NO solutions aer found!
  xs <- xsample(t(AA),BB,EE,FF,GG,HH, sdB=0.01, jmp=0.025)$X
  pairs(xs, main= "Chemtax, Bayesian sample")

  dev.off()
  return(X)
  })
  
  
  X<-data.frame(t(X))
  
  X$Sample<-rownames(X)
  
  return(X)
}