\name{EMSNM-package}
\alias{EMSNM-package}
\alias{EMSNM}
\docType{package}
\title{
\packageTitle{EMSNM}
}
\description{
\packageDescription{EMSNM}
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{EMSNM}
\packageIndices{EMSNM}

The EMalgorithm is used to estimate the parameters, EMbootstrap is used to estimate the parameters with bootstrap method. In EMsimulation we can simulate the situation with given parameters, so parameter estimation can be verified.
}
\author{
\packageAuthor{EMSNM}

Maintainer: \packageMaintainer{EMSNM}
}

\keyword{ package }

\examples{
#parameter initialization
etasize <- 2
classsize <- 2
alphasize <- 3
samplesize <- 100
expriments <- 30
  
etatest <- matrix(c(1,1,
                    0,0),etasize,classsize)

alphatest <- matrix(c(1,0,2,
                      4,3,5),alphasize,classsize)
sigmatest <- 0.5

#test of EMsimulation
EMsimulation_result <- EMsimulation(eta=etatest,alpha=alphatest,sigma=sigmatest,
                            samplesize=samplesize,expriments=expriments,
                            compact_flag=TRUE,C0=5,C1=0.5,C2=5)
index <- which(EMsimulation_result$sigma<0.8)
EMsimulation_result_sort <- EM_result_sort(EMsimulation_result$alpha[index,,],
                                            EMsimulation_result$eta[index,,])
EM_parameter <- EM_parameter_sd(EMsimulation_result_sort$alpha,
                                EMsimulation_result_sort$eta,
                                EMsimulation_result$sigma[index])

#test of EMbootstrap
samplesize <- 1000
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
