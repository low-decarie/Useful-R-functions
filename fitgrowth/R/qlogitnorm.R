qlogitnorm <-
function(
	### Quantiles of logitnormal distribution.	
	p
	,mu = 0, sigma = 1	##<< distribution parameters
	,...
){
	##seealso<< \code{\link{logitnorm}}
	qn <- qnorm(p,mean=mu,sd=sigma,...)
	plogis(qn)
}
