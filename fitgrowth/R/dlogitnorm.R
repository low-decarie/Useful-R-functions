dlogitnorm <-
function(
	### Density function of logitnormal distribution	
	q		##<< quantiles
	,mu = 0, sigma = 1	##<< distribution parameters
	,...	##<< further arguments passed to \code{\link{dnorm}}: \code{mean}, and \code{sd} for mu and sigma respectively.  
){
	##alias<< logitnorm
	
	##details<< \describe{\item{Logitnorm distribution}{ 
	## \itemize{
	## \item{density function: dlogitnorm }
	## \item{distribution function: \code{\link{plogitnorm}} }
	## \item{quantile function: \code{\link{qlogitnorm}} }
	## \item{random generation function: \code{\link{rlogitnorm}} }
	## }
	## }}
	
	##details<< \describe{\item{Transformation functions}{ 
	## \itemize{
	## \item{ (0,1) -> (-Inf,Inf): \code{\link{logit}} }
	## \item{ (-Inf,Inf) -> (0,1): \code{\link{invlogit}} }
	## }
	## }}
	
	##details<< \describe{\item{Moments and mode}{ 
	## \itemize{
	## \item{ Expected value and variance: \code{\link{momentsLogitnorm}} }
	## \item{ Mode: \code{\link{modeLogitnorm}} }
	## }
	## }}
	
	##details<< \describe{\item{Estimating parameters}{ 
	## \itemize{
	## \item{from mode and upper quantile: \code{\link{twCoefLogitnormMLE}} }
	## \item{from median and upper quantile: \code{\link{twCoefLogitnorm}} }
	## \item{from expected value, i.e. mean and upper quantile: \code{\link{twCoefLogitnormE}} }
	## \item{from a confidence interval which is symmetric at normal scale: \code{\link{twCoefLogitnormCi}} }
	## \item{from prescribed quantiles: \code{\link{twCoefLogitnormN}} }
	## }
	## }}
	
	ql <- qlogis(q)
	dnorm(ql,mean=mu,sd=sigma,...) /q/(1-q)	# multiply by the Jacobian (derivative) of the back-Transformation (logit)
}
