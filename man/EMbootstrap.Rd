\name{EMbootstrap}
\alias{EMbootstrap}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bootstrap Method
}
\description{
Estimate the value of parameters several times by bootstrap method where the parameters are estimated by EM algorithm. In this way, we can observe the distribution of parameter.
}
\usage{
EMbootstrap(X, Y, Z, samplesize, boots_samplesize, boots_expriments, classsize = 2,
            learning_rate = 0.1, regular_parameter_eta = 0.001, max_iteration = 10000, 
            max_iteration_eta = 10000, compact_flag = FALSE, C0 = 5, C1 = 2, C2 = 9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{X}{
  the co-variables of the mean of each subgroup
}
  \item{Y}{
  the respond variable
}
  \item{Z}{
  the co-variables determining subgroup
}
  \item{samplesize}{
  the size of this sample.
}
  \item{boots_samplesize}{
  the size of chosen sample in one step of bootstrap
}
  \item{boots_expriments}{
  the steps of bootstrap
}
  \item{classsize}{
  the number of subgroup types in your model assumption
}
  \item{learning_rate}{
  learning rate of updating eta
}
  \item{regular_parameter_eta}{
  regular value of updating eta by gradient descending method.
}
  \item{max_iteration}{
  maximum steps of iteration to avoid unlimited looping. 
}
  \item{max_iteration_eta}{
  maximal steps of eta iteration to avoid unlimited looping. 
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
\details{
Actually, the method can be extended to other parameter estimation where the standard error of parameter can't be calculated in a simple way.
}
\value{
\item{alpha}{alpha estimated by bootstrap method.}
\item{eta}{eta estimated by bootstrap method.}
\item{sigma}{sigma estimated by bootstrap method.}
}

\author{
Linsui Deng
}


\examples{
#parameter initialization
etasize <- 2
classsize <- 2
alphasize <- 3
samplesize <- 1000
  
etatest <- matrix(c(1,1,
                    0,0),etasize,classsize)

alphatest <- matrix(c(1,0,2,
                      4,3,5),alphasize,classsize)
sigmatest <- 0.5

#test of EMbootstrap
X <- matrix(c(matrix(1,samplesize),
                  rnorm(samplesize*(etasize-1))+1),samplesize,etasize)
Z <- matrix(c(matrix(1,samplesize),rbinom(prob=1/2,size=1,n=samplesize),
                 rnorm(samplesize*(alphasize-2))+1),samplesize,alphasize)
                 
Wtest <- Wgenerate(alpha=alphatest,eta=etatest,sigma=sigmatest,X=X,Z=Z)

boots_samplesize <- 100
boots_expriments <- 30
samplesize <- dim(Wtest$X)[1]
EMbootstrap_theta <- EMbootstrap(Wtest$X,Wtest$Y,Wtest$Z,samplesize,
                                 boots_samplesize,boots_expriments,
                                 classsize=2,compact_flag=TRUE,C0=5,C1=0.2,C2=5)
index <- which(EMbootstrap_theta$sigma<0.8)
EMsimulation_result_sort <- EM_result_sort(EMbootstrap_theta$alpha[index,,],
                                             EMbootstrap_theta$eta[index,,])
EM_parameter <- EM_parameter_sd(EMsimulation_result_sort$alpha,
                                EMsimulation_result_sort$eta,
                                 EMbootstrap_theta$sigma[index]) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{nonparametric}% use one of  RShowDoc("KEYWORDS")
