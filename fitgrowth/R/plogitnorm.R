plogitnorm <-
function(
	### Distribution function for logitnormal distribution	
	q
	,mu = 0, sigma = 1	##<< distribution parameters
	,...
){
	##seealso<< \code{\link{logitnorm}}
	ql <- qlogis(q)
	pnorm(ql,mean=mu,sd=sigma,...)
}
