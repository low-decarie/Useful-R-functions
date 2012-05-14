\name{twCoefLogitnormMLE}
\alias{twCoefLogitnormMLE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twCoefLogitnormMLE(mle, quant, perc = 0.999)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mle}{
%%     ~~Describe \code{mle} here~~
}
  \item{quant}{
%%     ~~Describe \code{quant} here~~
}
  \item{perc}{
%%     ~~Describe \code{perc} here~~
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
structure(function (mle, quant, perc = 0.999) 
{
    nc <- c(length(mle), length(quant), length(perc))
    n <- max(nc)
    res <- matrix(as.numeric(NA), n, 2, dimnames = list(NULL, 
        c("mu", "sigma")))
    for (i in 1:n) {
        i0 <- i - 1
        mleI <- mle[1 + i0\%\%nc[1]]
        if (mleI == 0.5) 
            mleI = 0.5 - .Machine$double.eps
        logitMle <- logit(mleI)
        upper <- abs(logitMle) - .Machine$double.eps
        tmp <- sign(logitMle) * log(seq(1, exp(upper), length.out = 40))
        oftmp <- .ofLogitnormMLE(tmp, mle = (mleI), logitMle = logitMle, 
            quant = (quantI <- quant[1 + i0\%\%nc[2]]), perc = (percI <- perc[1 + 
                i0\%\%nc[3]]))
        imin <- which.min(oftmp)
        intv <- tmp[c(max(1, imin - 1), min(length(tmp), imin + 
            1))]
        if (diff(intv) == 0) 
            mu <- intv[1]
        else mu <- optimize(.ofLogitnormMLE, interval = intv, 
            mle = (mleI), logitMle = logitMle, quant = (quantI <- quant[1 + 
                i0\%\%nc[2]]), perc = (percI <- perc[1 + i0\%\%nc[3]]))$minimum
        sigma <- sqrt((logitMle - mu)/(2 * mleI - 1))
        res[i, ] <- c(mu, sigma)
    }
    res
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
