logit <-
function(
	### Transforming (0,1) to normal scale (-Inf Inf)
	p,...
){ 
	##details<<
	## function \eqn{\operatorname{logit}(p)=\log\left( \frac{p}{1-p} \right) =\log(p)-\log(1-p). \!\,}
	##seealso<< \code{\link{invlogit}}
	##seealso<< \code{\link{logitnorm}}
	qlogis(p,...) 
}
