invlogit <-
function(
	### Transforming (-Inf,Inf) to original scale (0,1)
	q,...
){
	##details<<
	## function \eqn{f(z) = \frac{e^{z}}{e^{z} + 1} \! = \frac{1}{1 + e^{-z}} \!}
	##seealso<< \code{\link{logit}}
	##seealso<< \code{\link{logitnorm}}
	plogis(q,...) 
}
