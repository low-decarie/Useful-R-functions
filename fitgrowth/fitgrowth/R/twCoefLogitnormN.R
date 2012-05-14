twCoefLogitnormN <-
structure(function(
	### Estimating coefficients of logitnormal distribution from a vector of quantiles and perentiles (non-vectorized).	
	quant					##<< the quantile values
	,perc=c(0.5,0.975)		##<< the probabilites for which the quantiles were specified
	,method="BFGS"			##<< method of optimization (see \code{\link{optim}})
	,theta0=c(mu=0,sigma=1)	##<< starting parameters
	,returnDetails=FALSE	##<< if TRUE, the full output of optim is returned instead of only entry par
	, ... 					##<< further parameters passed to optim, e.g. control=list(maxit=1000)
){
	##seealso<< \code{\link{logitnorm}}
	names(theta0) <- c("mu","sigma")
	tmp <- optim( theta0, .ofLogitnorm, quant=quant,perc=perc, method=method, ...)
	if( tmp$convergence != 0)
		warning(paste("coefLogitnorm: optim did not converge. theta0=",paste(theta0,collapse=",")))
	if( returnDetails )
		tmp
	else
		tmp$par
	### named numeric vector with estimated parameters of the logitnormal distrubtion.
	### names: \code{c("mu","sigma")}
}, ex = function(){
	# estimate the parameters
	quant=c(0.7,0.8,0.9)
	perc=c(0.5,0.75,0.975)
	(theta <- twCoefLogitnormN( quant=quant, perc=perc ))
	
	x <- seq(0,1,length.out=41)[-c(1,41)]	# plotting grid
	px <- plogitnorm(x,mu=theta[1],sigma=theta[2])	#percentiles function
	plot(px~x); abline(v=quant,col="gray"); abline(h=perc,col="gray")
})
