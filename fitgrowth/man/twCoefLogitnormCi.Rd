\name{twCoefLogitnormCi}
\alias{twCoefLogitnormCi}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twCoefLogitnormCi(lower, upper, perc = 0.975, sigmaFac = qnorm(perc), isTransScale = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lower}{
%%     ~~Describe \code{lower} here~~
}
  \item{upper}{
%%     ~~Describe \code{upper} here~~
}
  \item{perc}{
%%     ~~Describe \code{perc} here~~
}
  \item{sigmaFac}{
%%     ~~Describe \code{sigmaFac} here~~
}
  \item{isTransScale}{
%%     ~~Describe \code{isTransScale} here~~
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
structure(function (lower, upper, perc = 0.975, sigmaFac = qnorm(perc), 
    isTransScale = FALSE) 
{
    if (!isTRUE(isTransScale)) {
        lower <- logit(lower)
        upper <- logit(upper)
    }
    halfWidth <- (upper - lower)/2
    sigma <- halfWidth/sigmaFac
    cbind(mu = upper - halfWidth, sigma = sigma)
  }, ex = function () 
{
    mu = 2
    sd = c(1, 0.8)
    p = 0.99
    lower <- l <- qlogitnorm(1 - p, mu, sd)
    upper <- u <- qlogitnorm(p, mu, sd)
    cf <- twCoefLogitnormCi(lower, upper)
    all.equal(cf[, "mu"], c(mu, mu))
    all.equal(cf[, "sigma"], sd)
  })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
