\name{twCoefLogitnorm}
\alias{twCoefLogitnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twCoefLogitnorm(median, quant, perc = 0.975, method = "BFGS", theta0 = c(mu = 0, sigma = 1), returnDetails = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{median}{
%%     ~~Describe \code{median} here~~
}
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
structure(function (median, quant, perc = 0.975, method = "BFGS", 
    theta0 = c(mu = 0, sigma = 1), returnDetails = FALSE, ...) 
{
    names(theta0) <- c("mu", "sigma")
    nc <- c(length(median), length(quant), length(perc))
    n <- max(nc)
    res <- matrix(as.numeric(NA), n, 2, dimnames = list(NULL, 
        c("mu", "sigma")))
    resOptim <- list()
    qmat <- cbind(median, quant)
    pmat <- cbind(0.5, perc)
    for (i in 1:n) {
        i0 <- i - 1
        tmp <- optim(theta0, .ofLogitnorm, perc = pmatI <- as.numeric(pmat[1 + 
            i0\%\%nrow(pmat), ]), quant = (qmatI <- qmat[1 + i0\%\%nrow(qmat), 
            ]), method = method, ...)
        if (tmp$convergence == 0) 
            res[i, ] <- tmp$par
        else warning(paste("coefLogitnorm: optim did not converge. theta0=", 
            paste(theta0, collapse = ","), "; median=", qmatI[1], 
            "; quant=", qmatI[2], "; perc=", pmatI[2], sep = ""))
        resOptim[[i]] <- tmp
    }
    if (returnDetails) 
        attr(res, "resOptim") <- resOptim
    res
  }, ex = function () 
{
    (theta <- twCoefLogitnorm(0.7, 0.9))
    x <- seq(0, 1, length.out = 41)[-c(1, 41)]
    px <- plogitnorm(x, mu = theta[1], sigma = theta[2])
    plot(px ~ x)
    abline(v = c(0.7, 0.9), col = "gray")
    abline(h = c(0.5, 0.975), col = "gray")
    dx <- dlogitnorm(x, mu = theta[1], sigma = theta[2])
    plot(dx ~ x)
    abline(v = c(0.7, 0.9), col = "gray")
    (theta <- twCoefLogitnorm(seq(0.4, 0.8, by = 0.1), 0.9))
  })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
