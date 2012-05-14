\name{exponential.growth.nls}
\alias{exponential.growth.nls}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
exponential.growth.nls(readings)
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
    fitted.readings <- readings
    ABS <- readings$ABS
    exp.model <- try(nls(formula = ABS ~ ABS0 * 2^(growth.rate * 
        Time), start = list(ABS0 = 0.1, growth.rate = 1), data = readings, 
        na.action = na.exclude, algorithm = "port", lower = c(0, 
            0), upper = Inf))
    if (class(exp.model) == "try-error") {
        fitted.readings$exp.N0 <- NA
        fitted.readings$exp.r <- NA
        fitted.readings$exp.predicted <- NA
    }
    else {
        exp.parameters <- t(data.frame(coef(exp.model, matrix = T)))
        interval <- confint(exp.model)
        fitted.readings$exp.N0 <- exp.parameters[1]
        fitted.readings$exp.r <- exp.parameters[2]
        fitted.readings$exp.r.lower <- interval["growth.rate", 
            "2.5\%"]
        fitted.readings$exp.r.upper <- interval["growth.rate", 
            "97.5\%"]
        fitted.readings$exp.predicted <- predict(exp.model)
    }
    return(fitted.readings)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
