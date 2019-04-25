\name{standard}
\alias{standard}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Data Standardlization
}
\description{
Standardlize the data to elimilate the effect of scale
}
\usage{
standard(X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  the original data
}
}

\value{
  standarlized data
}

\author{
Linsui Deng
}


\examples{
#data generata

Y <- rnorm(100)*2+5

Y_sta <- standard(Y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{algebra}% use one of  RShowDoc("KEYWORDS")
\keyword{array}
