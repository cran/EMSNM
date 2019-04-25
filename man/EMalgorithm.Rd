\name{EMalgorithm}
\alias{EMalgorithm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Parameter Estimation
}
\description{
Estimate paramters value by EM algorithm under the assumption of Sigmoid-Normal Model.
}
\usage{
EMalgorithm(X, Y, Z, etat, alphat, sigmat, classsize = 2, learning_rate = 0.1, 
            regular_parameter_eta = 0.001, max_iteration = 10000,
            max_iteration_eta = 10000, compact_flag = FALSE, C0 = 5, C1 = 2, C2 = 9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  the covariables of the mean of each subgroup
}
  \item{Y}{
  the respond variable
}
  \item{Z}{
  the covaraibles determining subgroup
}
  \item{etat}{
  the coeffients determining subgroup 
}
  \item{alphat}{
  the coeffients of the mean of each subgroup
}
  \item{sigmat}{
  the variance of Y
}
  \item{classsize}{
  the number of subgroup types in your model assumption
}
  \item{learning_rate}{
  learning rate of updating eta
}
  \item{regular_parameter_eta}{
  regular value of updating eta by gradiant descending methond.
}
  \item{max_iteration}{
  maximum steps of interation to avoid unlimited looping. 
}
  \item{max_iteration_eta}{
  maximal steps of eta interation to avoid unlimited looping. 
}
  \item{compact_flag}{
  if the value of eta is limited in a compact set, set it TRUE
}
  \item{C0}{
  the maximum of intercept of eta.
}
  \item{C1}{
  the minimum of the norm of slope of eta
}
  \item{C2}{
  the maximum of the norm of slope of eta
}
}

\value{
\item{alpha}{alpha estimation}
\item{eta}{eta estimation}
\item{sigma}{sigma estimation}
}

\author{
Linsui Deng
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


\examples{
#data generation
samplesize <- 1000
classsize <- 2
etasize <- 3
alphasize <- 3

set.seed(1)
Xtest <- data.frame(matrix(rnorm(samplesize*etasize),samplesize,etasize))
etatest <- matrix(c(1,2,-1,
                    0,0,0),etasize,classsize)

Ztest <- matrix(rnorm(samplesize*alphasize),samplesize,alphasize)
alphatest <- matrix(c(1,0,2,
                      5,0,7),alphasize,classsize)
sigmatest <- 5

Wtest <- Wgenerate(alpha=alphatest,eta=etatest,X=Xtest,Z=Ztest,sigma=sigmatest)

eta_initial <- matrix(c(rnorm(3),0,0,0),etasize,classsize)
alpha_initial<- matrix(rnorm(alphasize*classsize)*3,alphasize,classsize)
sigma_initial <- 1

EMtheta <- EMalgorithm(X=Wtest$X,Z=Wtest$Z,Y=Wtest$Y,classsize=2,
                       etat=eta_initial,alphat=alpha_initial,sigmat=sigma_initial,
                       learning_rate=0.01,regular_parameter_eta=0.001,
                       max_iteration=1000,max_iteration_eta=10000, 
                       compact_flag = TRUE, C0 = 5, C1 = 2, C2 = 9)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{multivariate}% use one of  RShowDoc("KEYWORDS")
\keyword{models}% __ONLY ONE__ keyword per line
\keyword{nonlinear}