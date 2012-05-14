twCoefLogitnormE <-
structure(function(
	### Estimating coefficients of logitnormal distribution from expected value, i.e. mean, and upper quantile.	
	mean					##<< the expected value of the density function
	,quant					##<< the quantile values
	,perc=c(0.975)			##<< the probabilites for which the quantiles were specified
	,method="BFGS"			##<< method of optimization (see \code{\link{optim}})
	,theta0=c(mu=0,sigma=1)	##<< starting parameters
	,returnDetails=FALSE	##<< if TRUE, the full output of optim is returned with attribut resOptim
	, ... 
){
	##seealso<< \code{\link{logitnorm}}
	# twCoefLogitnormE
	names(theta0) <- c("mu","sigma")
	mqp <- cbind(mean,quant,perc)
	n <- nrow(mqp)
	res <- matrix( as.numeric(NA), n,2, dimnames=list(NULL,c("mu","sigma")))
	resOptim <- list()
	for( i in 1:n ){
		mqpi<- mqp[i,]
		tmp <- optim( theta0, .ofLogitnormE, mean=mqpi[1], quant=mqpi[2], perc=mqpi[3], method=method, ...)
		resOptim[[i]] <- tmp
		if( tmp$convergence != 0)
			warning(paste("coefLogitnorm: optim did not converge. theta0=",paste(theta0,collapse=",")))
		else
			res[i,] <- tmp$par
	}
	if( returnDetails )
		attr(res,"resOptim") <- resOptim
	res
	### named numeric matrix with estimated parameters of the logitnormal distrubtion.
	### colnames: \code{c("mu","sigma")}
}, ex = function(){
	# estimate the parameters
	(thetaE <- twCoefLogitnormE(0.7,0.9))
	
	x <- seq(0,1,length.out=41)[-c(1,41)]	# plotting grid
	px <- plogitnorm(x,mu=thetaE[1],sigma=thetaE[2])	#percentiles function
	plot(px~x); abline(v=c(0.7,0.9),col="gray"); abline(h=c(0.5,0.975),col="gray")
	dx <- dlogitnorm(x,mu=thetaE[1],sigma=thetaE[2])	#density function
	plot(dx~x); abline(v=c(0.7,0.9),col="gray")
	
	z <- rlogitnorm(1e5, mu=thetaE[1],sigma=thetaE[2])
	mean(z)	# about 0.7
	
	# vectorized
	(theta <- twCoefLogitnormE(mean=seq(0.4,0.8,by=0.1),quant=0.9))
})
