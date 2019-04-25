\name{fnorm}
\alias{fnorm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Density Value
}
\description{
Calculate the density value of respond value Y under each mean and homogeneous variance.
}
\usage{
fnorm(Y, mu, sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Y}{
  the respond variable
}
  \item{mu}{
  different mean of each subgroup
}
  \item{sigma}{
  standard error
}
}


\value{
the density value of Y under different mu and common sigma.
}

\author{
Linsui Deng
}

\examples{
fnormtest <- fnorm(matrix(1:6,3,2),matrix(seq(1,3,length=6),3,2),1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}% use one of  RShowDoc("KEYWORDS")
