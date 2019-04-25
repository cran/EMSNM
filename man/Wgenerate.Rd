\name{Wgenerate}
\alias{Wgenerate}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sigmoid Logistic Data Generation
}
\description{
Generate data satisties Sigmoid Logistic Model to check EMalgorithm.
}
\usage{
Wgenerate(alpha, sigma = 1, eta, samplesize = 0, X, Z, seed1 = 0, seed2 = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alpha}{
  the coeffients of the mean of each subgroup
}
  \item{sigma}{
  the variance of Y
}
  \item{eta}{
  the coeffients determining subgroup 
}
  \item{samplesize}{
  the size of sample you wanna generate
}
  \item{X}{
  the covariables of the mean of each subgroup
}
  \item{Z}{
  the covaraibles determining subgroup
}
  \item{seed1}{
  random seed of generating Y
}
  \item{seed2}{
  random seed of generating G
}
}

\value{
\item{X}{the covariables of the mean of each subgroup}
\item{Z}{the covaraibles determining subgroup}
\item{Y}{the generated respond variable}
\item{G}{the classes items belonging to}
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

#test of Wgenerate
Xtest <- data.frame(matrix(rnorm(samplesize*etasize),samplesize,etasize))
Ztest <- matrix(rnorm(samplesize*alphasize),samplesize,alphasize)

etatest <- matrix(seq(1.15,1,length=etasize*classsize),etasize,classsize)
alphatest <- matrix(seq(1.15,1,length=alphasize*classsize),alphasize,classsize)

Wtest <- Wgenerate(alpha=alphatest,eta=etatest,X=Xtest,Z=Ztest)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{datagen}% use one of  RShowDoc("KEYWORDS")
