\name{softmax}
\alias{softmax}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Softmax Value
}
\description{
Calculate the Softmax Value of each subgroup to represent the probability of items belonging to specific class.
}
\usage{
softmax(eta, X)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eta}{
  the coeffients determining subgroup 
}
  \item{X}{
  the covariables determining subgroup 
}
}

\value{
Softmax Value of each subgroup
}

\author{
Linsui Deng
}

\examples{
#some variables
samplesize <- 1000
classsize <- 6
etasize <- 3
alphasize <- 2

#test of softmax
Xtest <- data.frame(matrix(rnorm(samplesize*etasize),samplesize,etasize))
etatest <- matrix(seq(1.15,1,length=etasize*classsize),etasize,classsize)
softmax_value <- softmax(etatest,Xtest)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{algebra}% use one of  RShowDoc("KEYWORDS")
\keyword{array}