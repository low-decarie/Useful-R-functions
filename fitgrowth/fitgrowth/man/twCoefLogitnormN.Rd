\name{twCoefLogitnormN}
\alias{twCoefLogitnormN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twCoefLogitnormN(quant, perc = c(0.5, 0.975), method = "BFGS", theta0 = c(mu = 0, sigma = 1), returnDetails = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{quant}{
%%     ~~Describe \code{quant} here~~
}
  \item{perc}{
%%     ~~Describe \code{perc} here~~
}
  \item{method}{
%%     ~~Describe \code{method} here~~
}
  \item{theta0}{
%%     ~~Describe \code{theta0} here~~
}
  \item{returnDetails}{
%%     ~~Describe \code{returnDetails} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (quant, perc = c(0.5, 0.975), method = "BFGS", 
    theta0 = c(mu = 0, sigma = 1), returnDetails = FALSE, ...) 
{
    names(theta0) <- c("mu", "sigma")
    tmp <- optim(theta0, .ofLogitnorm, quant = quant, perc = perc, 
        method = method, ...)
    if (tmp$convergence != 0) 
        warning(paste("coefLogitnorm: optim did not converge. theta0=", 
            paste(theta0, collapse = ",")))
    if (returnDetails) 
        tmp
    else tmp$par
  }, ex = function () 
{
    quant = c(0.7, 0.8, 0.9)
    perc = c(0.5, 0.75, 0.975)
    (theta <- twCoefLogitnormN(quant = quant, perc = perc))
    x <- seq(0, 1, length.out = 41)[-c(1, 41)]
    px <- plogitnorm(x, mu = theta[1], sigma = theta[2])
    plot(px ~ x)
    abline(v = quant, col = "gray")
    abline(h = perc, col = "gray")
  })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
