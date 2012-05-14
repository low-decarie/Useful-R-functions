\name{twSigmaLogitnorm}
\alias{twSigmaLogitnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twSigmaLogitnorm(mle, mu = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mle}{
%%     ~~Describe \code{mle} here~~
}
  \item{mu}{
%%     ~~Describe \code{mu} here~~
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
structure(function (mle, mu = 0) 
{
    sigma = sqrt((logit(mle) - mu)/(2 * mle - 1))
    cbind(mu = mu, sigma = sigma)
  }, ex = function () 
{
    (theta <- twCoefLogitnormMLE(0.7, 0.9))
    x <- seq(0, 1, length.out = 41)[-c(1, 41)]
    px <- plogitnorm(x, mu = theta[1], sigma = theta[2])
    plot(px ~ x)
    abline(v = c(0.7, 0.9), col = "gray")
    abline(h = c(0.999), col = "gray")
    dx <- dlogitnorm(x, mu = theta[1], sigma = theta[2])
    plot(dx ~ x)
    abline(v = c(0.7, 0.9), col = "gray")
    (theta <- twCoefLogitnormMLE(mle = seq(0.4, 0.8, by = 0.1), 
        quant = 0.9))
  })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
