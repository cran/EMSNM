\name{EMsimulation}
\alias{EMsimulation}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulation For Estimation
}
\description{
It simulates the experiments with given alpha, eta and sigma to verify the EMalgorithm
}
\usage{
EMsimulation(eta, alpha, sigma, samplesize, expriments, 
             compact_flag = FALSE, C0 = 5, C1 = 2, C2 = 9)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{eta}{
  the true value of eta
}
  \item{alpha}{
  the true value of alpha
}
  \item{sigma}{
  the true value of sigma
}
  \item{samplesize}{
  the size of sample
}
  \item{expriments}{
  the times of experiments
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
\item{alpha}{alpha estimated in simulation.}
\item{eta}{eta estimated in simulation.}
\item{sigma}{sigma estimated in simulation.}
}

\author{
Linsui Deng
}


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
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{design}% use one of  RShowDoc("KEYWORDS")
