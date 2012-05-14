\name{logistic.growth.nls}
\alias{logistic.growth.nls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
logistic.growth.nls(readings, upper = 10, printer = T, do.interval = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{readings}{
%%     ~~Describe \code{readings} here~~
}
  \item{upper}{
%%     ~~Describe \code{upper} here~~
}
  \item{printer}{
%%     ~~Describe \code{printer} here~~
}
  \item{do.interval}{
%%     ~~Describe \code{do.interval} here~~
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
function (readings, upper = 10, printer = T, do.interval = T) 
{
    if (printer) {
        print(unique(readings$culture))
    }
    fitted.readings <- readings
    start.values = c(K = 1, r = 1, N0 = 0.01)
    culture.model <- try(nls(formula = ABS ~ (K * N0 * exp(r * 
        Time))/(K + N0 * (exp(r * Time) - 1)), data = readings, 
        start = start.values, na.action = na.exclude, algorithm = "port", 
        lower = c(0, 0, 0), upper = c(upper, Inf, Inf)))
    if (class(culture.model) == "try-error") {
        fitted.readings$logistic.nls.N0 <- NA
        fitted.readings$logistic.nls.K <- NA
        fitted.readings$logistic.nls.K.lower <- NA
        fitted.readings$logistic.nls.K.upper <- NA
        fitted.readings$logistic.nls.r <- NA
        fitted.readings$logistic.nls.r.lower <- NA
        fitted.readings$logistic.nls.r.upper <- NA
        fitted.readings$logistic.nls.predicted <- NA
    }
    else {
        parameters <- coef(culture.model)
        fitted.readings$logistic.nls.N0 <- parameters["N0"]
        fitted.readings$logistic.nls.K <- parameters["K"]
        fitted.readings$logistic.nls.r <- parameters["r"]
        fitted.readings$logistic.nls.predicted <- predict(culture.model)
        interval <- try(confint(culture.model))
        if (any(class(interval) == "try-error", !do.interval)) {
            fitted.readings$logistic.nls.K.lower <- NA
            fitted.readings$logistic.nls.K.upper <- NA
            fitted.readings$logistic.nls.r.lower <- NA
            fitted.readings$logistic.nls.r.upper <- NA
        }
        else {
            fitted.readings$logistic.nls.r.lower <- interval["r", 
                "2.5\%"]
            fitted.readings$logistic.nls.r.upper <- interval["r", 
                "97.5\%"]
            fitted.readings$logistic.nls.K.lower <- interval["K", 
                "2.5\%"]
            fitted.readings$logistic.nls.K.upper <- interval["K", 
                "97.5\%"]
        }
    }
    return(fitted.readings)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
