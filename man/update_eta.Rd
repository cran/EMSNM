\name{update_eta}
\alias{update_eta}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Updata Eta
}
\description{
Updata eta in step t+1 with given data and coeffients estimated in step t.
}
\usage{
update_eta(fun, alphat, sigmat, etat, X, Y, Z, learning_rate_eta = 0.001, 
           regular_parameter_eta = 0.001, max_iteration_eta = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{fun}{
  the function updata eta
}
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
  \item{learning_rate_eta}{
  learning rate of updating eta
}
  \item{regular_parameter_eta}{
  regular value of updating eta by gradiant descending methond.
}
  \item{max_iteration_eta}{
  maximal steps of eta interation to avoid unlimited looping.
}
}

\value{
\item{alpha}{alpha estimated in step t.}
\item{eta}{eta estimated in step t+1.}
\item{sigma}{sigma estimated in step t.}
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

#test of update_eta
thetaupdate_eta <- update_eta(fun=eta_gradient_fun,alphat=alphatest,sigmat=sigmatest,
           etat=etatest,X=Wtest$X,Z=Wtest$Z,Y=Wtest$Y,
           learning_rate=0.1,regular_parameter=0.001,max_iteration=10000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}% use one of  RShowDoc("KEYWORDS")
\keyword{models}% __ONLY ONE__ keyword per line
\keyword{nonlinear}
