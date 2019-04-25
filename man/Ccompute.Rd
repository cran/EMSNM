\name{Ccompute}
\alias{Ccompute}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Ccompute
}
\description{
Compute the probability of Y in given parameters alphat, sigmat, etat and variables X, Z by the Bayesian Formula under the assumption of Sigmoid-Normal Model.
}
\usage{
Ccompute(alphat, sigmat, etat, X, Z, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alphat}{
  the coeffients of the mean of each subgroup
}
  \item{sigmat}{
  the variance of Y
}
  \item{etat}{
  the coeffients determining subgroup 
}
  \item{X}{
  the covariables of the mean of each subgroup
}
  \item{Z}{
  the covaraibles determining subgroup
}
  \item{Y}{
  the respond variable
}
}


\value{
the probability of Y in given parameters alphat, sigmat, etat and variables X, Z under the assumption of Sigmoid-Normal Model.
}


\author{
Linsui Deng
}



%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
#some variables
samplesize <- 1000
classsize <- 6
etasize <- 3
alphasize <- 2

Xtest <- data.frame(matrix(rnorm(samplesize*etasize),samplesize,etasize))
Ztest <- matrix(rnorm(samplesize*alphasize),samplesize,alphasize)

etatest <- matrix(seq(1.15,1,length=etasize*classsize),etasize,classsize)
alphatest <- matrix(seq(1.15,1,length=alphasize*classsize),alphasize,classsize)

Wtest <- Wgenerate(alpha=alphatest,eta=etatest,X=Xtest,Z=Ztest)

#test of Ccompute
sigmatest <- 1
Ctest <- 
  Ccompute(alphat=alphatest,sigmat=sigmatest,
           etat=etatest,X=Wtest$X,Z=Wtest$Z,Y=Wtest$Y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{math}% use one of  RShowDoc("KEYWORDS")
