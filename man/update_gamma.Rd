\name{update_gamma}
\alias{update_gamma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Updata Alpha and Sigma
}
\description{
Updata alpha and sigma in t+1 step with given data and coeffients estimated in step t.
}
\usage{
update_gamma(alphat, sigmat, etat, X, Z, Y)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{alphat}{
  the estimated coeffients of the mean of each subgroup in step t
}
  \item{sigmat}{
  the estimated standard error of Y in step t
}
  \item{etat}{
  the estimated coeffients determining subgroup in step t
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
\item{alpha}{alpha estimated in step t+1.}
\item{eta}{eta estimated in step t.}
\item{sigma}{sigma estimated in step t+1.}
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

Xtest <- data.frame(matrix(rnorm(samplesize*etasize),samplesize,etasize))
Ztest <- matrix(rnorm(samplesize*alphasize),samplesize,alphasize)

etatest <- matrix(seq(1.15,1,length=etasize*classsize),etasize,classsize)
alphatest <- matrix(seq(1.15,1,length=alphasize*classsize),alphasize,classsize)
sigmatest <- 0.1

Wtest <- Wgenerate(alpha=alphatest,eta=etatest,X=Xtest,Z=Ztest)

#test of updata_gamma
thetaupdate_gamma <- update_gamma(alphat=alphatest,sigmat=sigmatest,
                            etat=etatest,X=Wtest$X,Z=Wtest$Z,Y=Wtest$Y)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{models}% __ONLY ONE__ keyword per line
\keyword{linear}
