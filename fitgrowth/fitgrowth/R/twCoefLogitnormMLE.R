twCoefLogitnormMLE <-
structure(function(
	### Estimating coefficients of logitnormal distribution from mode and upper quantile	
	mle						##<< numeric vector: the mode of the density function
	,quant					##<< numeric vector: the upper quantile value
	,perc=0.999				##<< numeric vector: the probability for which the quantile was specified
	#, ... 					##<< Further parameters to \code{\link{optimize}}
){
	##seealso<< \code{\link{logitnorm}}
	# twCoefLogitnormMLE
	nc <- c(length(mle),length(quant),length(perc)) 
	n <- max(nc)
	res <- matrix( as.numeric(NA), n,2, dimnames=list(NULL,c("mu","sigma")))
	for( i in 1:n ){
		i0 <- i-1
		mleI<-mle[1+i0%%nc[1]]
		if( mleI==0.5) mleI=0.5-.Machine$double.eps	# in order to estimate sigma
		# we now that mu is in (logit(mle),0) for mle < 0.5 and in (0,logit(mle)) for mle > 0.5 for unimodal distribution
		# there might be a maximum in the middle and optimized misses the low part
		# hence, first get near the global minimum by a evaluating the cost at a grid
		# grid is spaced narrower at the edge
		logitMle <- logit(mleI)
		#intv <- if( logitMle < 0) c(logitMle+.Machine$double.eps,0) else c(0,logitMle-.Machine$double.eps)
		upper <- abs(logitMle)-.Machine$double.eps
		tmp <- sign(logitMle)*log(seq(1,exp(upper),length.out=40))
		oftmp<-.ofLogitnormMLE(tmp, mle=(mleI), logitMle=logitMle, quant=(quantI<-quant[1+i0%%nc[2]]), perc=(percI<-perc[1+i0%%nc[3]]))
		#plot(tmp,oftmp)
		imin <- which.min(oftmp)
		intv <- tmp[ c(max(1,imin-1), min(length(tmp),imin+1)) ]
		if( diff(intv)==0 ) mu <- intv[1] else
			mu <- optimize( .ofLogitnormMLE, interval=intv, mle=(mleI), logitMle=logitMle, quant=(quantI<-quant[1+i0%%nc[2]]), perc=(percI<-perc[1+i0%%nc[3]]))$minimum
		sigma <- sqrt((logitMle-mu)/(2*mleI-1))
		res[i,] <- c(mu,sigma)
	}
	res
	### numeric matrix with columns \code{c("mu","sigma")}
	### rows correspond to rows in mle, quant, and perc
}, ex = function(){
	
	# estimate the parameters, with mode 0.7 and upper quantile 0.9
	(theta <- twCoefLogitnormMLE(0.7,0.9))
	
	x <- seq(0,1,length.out=41)[-c(1,41)]	# plotting grid
	px <- plogitnorm(x,mu=theta[1],sigma=theta[2])	#percentiles function
	plot(px~x); abline(v=c(0.7,0.9),col="gray"); abline(h=c(0.999),col="gray")
	dx <- dlogitnorm(x,mu=theta[1],sigma=theta[2])	#density function
	plot(dx~x); abline(v=c(0.7,0.9),col="gray")
	
	# vectorized
	(theta <- twCoefLogitnormMLE(mle=seq(0.4,0.8,by=0.1),quant=0.9))
})
