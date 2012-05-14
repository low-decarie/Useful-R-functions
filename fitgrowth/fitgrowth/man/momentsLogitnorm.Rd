\name{momentsLogitnorm}
\alias{momentsLogitnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
momentsLogitnorm(mu, sigma, abs.tol = 0, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu}{
%%     ~~Describe \code{mu} here~~
}
  \item{sigma}{
%%     ~~Describe \code{sigma} here~~
}
  \item{abs.tol}{
%%     ~~Describe \code{abs.tol} here~~
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
structure(function (mu, sigma, abs.tol = 0, ...) 
{
    fExp <- function(x) plogis(x) * dnorm(x, mean = mu, sd = sigma)
    .exp <- integrate(fExp, -Inf, Inf, abs.tol = abs.tol, ...)$value
    fVar <- function(x) (plogis(x) - .exp)^2 * dnorm(x, mean = mu, 
        sd = sigma)
    .var <- integrate(fVar, -Inf, Inf, abs.tol = abs.tol, ...)$value
    c(mean = .exp, var = .var)
  }, ex = function () 
{
    (res <- momentsLogitnorm(4, 1))
    (res <- momentsLogitnorm(5, 0.1))
  })
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
