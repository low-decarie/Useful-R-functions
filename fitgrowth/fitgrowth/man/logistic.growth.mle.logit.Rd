\name{logistic.growth.mle.logit}
\alias{logistic.growth.mle.logit}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
logistic.growth.mle.logit(readings)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{readings}{
%%     ~~Describe \code{readings} here~~
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
function (readings) 
{
    print(unique(readings$culture))
    fitted.readings <- readings
    like.growth <- function(parameters = c(1, 1, 10^-10, 0.1), 
        t, Nt, ABS) {
        K <- parameters[1]
        r <- parameters[2]
        N0 <- parameters[3]
        beta <- parameters[4]
        Nt <- (K * N0 * exp(r * t))/(K + N0 * (exp(r * t) - 1))
        likelihood <- -sum(dnorm(x = logit((ABS - N0)/((1.5) - 
            N0)), mean = logit((Nt - N0)/((1.5) - N0)), sd = beta, 
            log = T))
        return(likelihood)
    }
    fit <- with(readings, optim(par = c(1, 1, 10^-10, 0.1), fn = like.growth, 
        t = Time, ABS = ABS))
    K <- fit$par[1]
    r <- fit$par[2]
    N0 <- fit$par[3]
    beta <- fit$par[4]
    t <- readings$Time
    predicted <- (K * N0 * exp(r * t))/(K + N0 * (exp(r * t) - 
        1))
    fitted.readings$N0 <- N0
    fitted.readings$K <- K
    fitted.readings$r <- r
    fitted.readings$predicted <- predicted
    return(fitted.readings)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
