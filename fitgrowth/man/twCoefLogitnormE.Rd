\name{twCoefLogitnormE}
\alias{twCoefLogitnormE}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
twCoefLogitnormE(mean, quant, perc = c(0.975), method = "BFGS", theta0 = c(mu = 0, sigma = 1), returnDetails = FALSE, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mean}{
%%     ~~Describe \code{mean} here~~
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
structure(function (mean, quant, perc = c(0.975), method = "BFGS", 
    theta0 = c(mu = 0, sigma = 1), returnDetails = FALSE, ...) 
{
    names(theta0) <- c("mu", "sigma")
    mqp <- cbind(mean, quant, perc)
    n <- nrow(mqp)
    res <- matrix(as.numeric(NA), n, 2, dimnames = list(NULL, 
        c("mu", "sigma")))
    resOptim <- list()
    for (i in 1:n) {
        mqpi <- mqp[i, ]
        tmp <- optim(theta0, .ofLogitnormE, mean = mqpi[1], quant = mqpi[2], 
            perc = mqpi[3], method = method, ...)
        resOptim[[i]] <- tmp
        if (tmp$convergence != 0) 
            warning(paste("coefLogitnorm: optim did not converge. theta0=", 
                paste(theta0, collapse = ",")))
        else res[i, ] <- tmp$par
    }
    if (returnDetails) 
        attr(res, "resOptim") <- resOptim
    res
  }, ex = function () 
{
    (thetaE <- twCoefLogitnormE(0.7, 0.9))
    x <- seq(0, 1, length.out = 41)[-c(1, 41)]
    px <- plogitnorm(x, mu = thetaE[1], sigma = thetaE[2])
    plot(px ~ x)
    abline(v = c(0.7, 0.9), col = "gray")
    abline(h = c(0.5, 0.975), col = "gray")
    dx <- dlogitnorm(x, mu = thetaE[1], sigma = thetaE[2])
    plot(dx ~ x)
    abline(v = c(0.7, 0.9), col = "gray")
    z <- rlogitnorm(1e+05, mu = thetaE[1], sigma = thetaE[2])
    mean(z)
    (theta <- twCoefLogitnormE(mean = seq(0.4, 0.8, by = 0.1), 
        quant = 0.9))
  })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
