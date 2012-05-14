twCoefLogitnorm <-
structure(function(
	### Estimating coefficients of logitnormal distribution from median and upper quantile	
	median					##<< numeric vector: the median of the density function
	,quant					##<< numeric vector: the upper quantile value
	,perc=0.975				##<< numeric vector: the probability for which the quantile was specified
	,method="BFGS"			##<< method of optimization (see \code{\link{optim}})
	,theta0=c(mu=0,sigma=1)	##<< starting parameters
	,returnDetails=FALSE	##<< if TRUE, the full output of optim is attached as attributes resOptim
	, ... 
){
	##seealso<< \code{\link{logitnorm}}
	# twCoefLogitnorm
	names(theta0) <- c("mu","sigma")
	nc <- c(length(median),length(quant),length(perc)) 
	n <- max(nc)
	res <- matrix( as.numeric(NA), n,2, dimnames=list(NULL,c("mu","sigma")))
	resOptim <- list()
	qmat <- cbind(median,quant)
	pmat <- cbind(0.5,perc)
	for( i in 1:n ){	# for each row in (recycled) vector
		i0 <- i-1
		tmp <- optim( theta0, .ofLogitnorm, perc=pmatI<-as.numeric(pmat[1+i0%%nrow(pmat),]), quant=(qmatI<-qmat[1+i0%%nrow(qmat),]), method=method, ...)
		if( tmp$convergence == 0)
			res[i,] <- tmp$par
		else
			warning(paste("coefLogitnorm: optim did not converge. theta0=",paste(theta0,collapse=","),"; median=",qmatI[1],"; quant=",qmatI[2],"; perc=",pmatI[2],sep=""))
		resOptim[[i]] <- tmp
	}
	if( returnDetails ) attr(res,"resOptim") <- resOptim
	res
	### numeric matrix with columns \code{c("mu","sigma")}
	### rows correspond to rows in median, quant, and perc
}, ex = function(){
	# estimate the parameters, with median at 0.7 and upper quantile at 0.9
	(theta <- twCoefLogitnorm(0.7,0.9))
	
	x <- seq(0,1,length.out=41)[-c(1,41)]	# plotting grid
	px <- plogitnorm(x,mu=theta[1],sigma=theta[2])	#percentiles function
	plot(px~x); abline(v=c(0.7,0.9),col="gray"); abline(h=c(0.5,0.975),col="gray")
	
	dx <- dlogitnorm(x,mu=theta[1],sigma=theta[2])	#density function
	plot(dx~x); abline(v=c(0.7,0.9),col="gray")
	
	# vectorized
	(theta <- twCoefLogitnorm(seq(0.4,0.8,by=0.1),0.9))
})
