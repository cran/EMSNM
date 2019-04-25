\name{Ggenerate}
\alias{Ggenerate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Subgroup Determination
}
\description{
In data genaration, determining the subgroup of each item belonging to through random number and Sigmoid Link function.
}
\usage{
Ggenerate(eta, X, seed = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eta}{
  the coeffients determining subgroup 
}
  \item{X}{
  the covariables determining subgroup 
}
  \item{seed}{
  random seed
}
}


\value{
the classes items belonging to, it's a vector. If X1 belongs to class 3, then the 1st row 3rd colmn is 1 and the rest of 1st row are 0. 
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

#test of Ggenerate
Xtest <- data.frame(matrix(rnorm(samplesize*etasize),samplesize,etasize))
etatest <- matrix(seq(1.15,1,length=etasize*classsize),etasize,classsize)

Gtest1 <- Ggenerate(etatest,Xtest)
Gtest2 <- Ggenerate(etatest,Xtest,1)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}% use one of  RShowDoc("KEYWORDS")
