rlogitnorm <-
function(
	### Random number generation for logitnormal distribution
	mu = 0, sigma = 1,	##<< distribution parameters
	...	##<< arguments to \code{\link{rnorm}}
){	
	##seealso<< \code{\link{logitnorm}}
	plogis( rnorm(mean=mu,sd=sigma,...) ) 
}
