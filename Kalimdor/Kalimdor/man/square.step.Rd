\name{square.step}
\alias{square.step}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
square.step(matrix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{matrix}{
%%     ~~Describe \code{matrix} here~~
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
function (matrix) 
{
    tL <- matrix[1, 1]
    tR <- matrix[1, ncol(matrix)]
    bL <- matrix[nrow(matrix), 1]
    bR <- matrix[nrow(matrix), ncol(matrix)]
    c <- matrix[mean(1:nrow(matrix)), mean(1:ncol(matrix))]
    t <- jitter(mean(tL, tR, c))
    b <- jitter(mean(bL, bR, c))
    l <- jitter(mean(tL, bL, c))
    r <- jitter(mean(tR, bR, c))
    matrix[1, mean(1:ncol(matrix))] <- t
    matrix[nrow(matrix), mean(1:ncol(matrix))] <- b
    matrix[mean(1:nrow(matrix)), 1] <- l
    matrix[mean(1:nrow(matrix)), ncol(matrix)] <- r
    return(matrix)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
