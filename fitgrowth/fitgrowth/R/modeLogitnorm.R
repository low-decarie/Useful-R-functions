modeLogitnorm <-
function(
	### Mode of the logitnormal distribution by numerical optimization
	mu	##<< parameter mu 
	,sigma		##<< parameter sigma
	,tol=invlogit(mu)/1000	##<< precisions of the estimate
){
	##seealso<< \code{\link{logitnorm}}
	itv <- if(mu<0) c(0,invlogit(mu)) else c(invlogit(mu),1)
	tmp <- optimize( .ofModeLogitnorm, interval=itv, mu=mu, sd2=sigma^2, tol=tol)
	tmp$minimum
}
