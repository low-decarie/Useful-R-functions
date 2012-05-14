twCoefLogitnormCi <-
structure(function( 
	### Calculates mu and sigma of the logitnormal distribution from lower and upper quantile, i.e. confidence interval.
	lower	##<< value at the lower quantile, i.e. practical minimum
	,upper	##<< value at the upper quantile, i.e. practical maximum
	,perc=0.975	##<< numeric vector: the probability for which the quantile was specified
	,sigmaFac=qnorm(perc) 	##<< sigmaFac=2 is 95% sigmaFac=2.6 is 99% interval
	,isTransScale = FALSE ##<< if true lower and upper are already on logit scale
){	
	##seealso<< \code{\link{logitnorm}}
	if( !isTRUE(isTransScale) ){
		lower <- logit(lower)
		upper <- logit(upper)
	}
	halfWidth <- (upper-lower)/2
	sigma <- halfWidth/sigmaFac
	cbind( mu=upper-halfWidth, sigma=sigma )
	### named numeric vector: mu and sigma parameter of the logitnormal distribution.
}, ex = function(){
	mu=2
	sd=c(1,0.8)
	p=0.99
	lower <- l <- qlogitnorm(1-p, mu, sd )		# p-confidence interval
	upper <- u <- qlogitnorm(p, mu, sd )		# p-confidence interval
	cf <- twCoefLogitnormCi(lower,upper)	
	all.equal( cf[,"mu"] , c(mu,mu) )
	all.equal( cf[,"sigma"] , sd )
})
