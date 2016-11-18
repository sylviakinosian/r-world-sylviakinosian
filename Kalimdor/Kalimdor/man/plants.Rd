\name{plants}
\alias{plants}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("plants")}
\format{
  The format is:
List of 4
 $ repro   : Named num [1:3] 0.2 0.4 0.6
  ..- attr(*, "names")= chr [1:3] "a" "b" "c"
 $ survival: Named num [1:3] 0.8 0.6 0.2
  ..- attr(*, "names")= chr [1:3] "a" "b" "c"
 $ comp.mat: num [1:3, 1:3] 0.5 0.3 0.8 0.3 0.5 0.1 0.8 0.1 0.5
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:3] "a" "b" "c"
  .. ..$ : chr [1:3] "a" "b" "c"
 $ names   : chr [1:3] "a" "b" "c"
 - attr(*, "class")= chr "plants"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(plants)
## maybe str(plants) ; plot(plants) ...
}
\keyword{datasets}
