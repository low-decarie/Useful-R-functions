\name{logistic.growth.mle.norm}
\alias{logistic.growth.mle.norm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
logistic.growth.mle.norm(readings, printer = F, upper = readings$upper)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{readings}{
%%     ~~Describe \code{readings} here~~
}
  \item{printer}{
%%     ~~Describe \code{printer} here~~
}
  \item{upper}{
%%     ~~Describe \code{upper} here~~
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
function (readings, printer = F, upper = readings$upper) 
{
    if (printer) {
        print(unique(readings$culture))
    }
    fitted.readings <- readings
    like.growth <- function(parameters = c(1, 1, 0.01, 0.1), 
        readings) {
        K <- parameters[1]
        r <- parameters[2]
        N0 <- parameters[3]
        st.dev <- parameters[4]
        ABS <- readings$ABS
        Time <- readings$Time
        Nt <- (K * N0 * exp(r * Time))/(K + N0 * (exp(r * Time) - 
            1))
        likelihood <- -sum(dnorm(ABS, Nt, sd = st.dev, log = T))
        if (any(c(Nt < 0, Nt > upper, K > upper, N0 < 0, r < 
            0, st.dev < 0, st.dev > upper))) {
            likelihood <- NA
        }
        return(likelihood)
    }
    try.test <- try({
        fit <- optim(par = c(1, 1, 0.01, 0.1), fn = like.growth, 
            readings = readings)
        K <- fit$par[1]
        r <- fit$par[2]
        N0 <- fit$par[3]
        Time <- readings$Time
        predicted <- (K * N0 * exp(r * Time))/(K + N0 * (exp(r * 
            Time) - 1))
        fitted.readings$logistic.mle.N0 <- N0
        fitted.readings$logistic.mle.K <- K
        fitted.readings$logistic.mle.r <- r
        fitted.readings$logistic.mle.predicted <- predicted
    })
    if (class(try.test) == "try-error") {
        fitted.readings$logistic.mle.N0 <- NA
        fitted.readings$logistic.mle.K <- NA
        fitted.readings$logistic.mle.r <- NA
        fitted.readings$logistic.mle.predicted <- NA
    }
    return(fitted.readings)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
