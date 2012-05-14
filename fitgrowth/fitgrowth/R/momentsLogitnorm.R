momentsLogitnorm <-
structure(function(
	### First two moments of the logitnormal distribution by numerical integration
	mu	##<< parameter mu 
	,sigma		##<< parameter sigma
	,abs.tol=0	##<< chaning default to \code{\link{integrate}}
	,...		##<< further parameters to the \code{\link{integrate}} function
){
	fExp <- function(x)  plogis(x)*dnorm(x,mean=mu,sd=sigma)
	.exp <- integrate(fExp,-Inf,Inf, abs.tol=abs.tol, ...)$value
	fVar <- function(x)   (plogis(x) - .exp)^2 * dnorm(x,mean=mu,sd=sigma)
	.var <- integrate(fVar,-Inf,Inf, abs.tol=abs.tol, ...)$value
	c( mean=.exp, var=.var )
	### named numeric vector with components \itemize{
	### \item{ \code{mean}: expected value, i.e. first moment}
	### \item{ \code{var}: variance, i.e. second moment }
	### }
}, ex = function(){
	(res <- momentsLogitnorm(4,1))
	(res <- momentsLogitnorm(5,0.1))
})
