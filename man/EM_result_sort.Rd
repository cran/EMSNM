\name{EM_result_sort}
\alias{EM_result_sort}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Sort Parameter
}
\description{
Since the number of subgroup types is always beyond 1, the order of subgroup may be different, we can sort them in this function.
}
\usage{
EM_result_sort(EMsimulation_result_alpha, EMsimulation_result_eta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{EMsimulation_result_alpha}{
  alpha estimated by bootstrap method.
}
  \item{EMsimulation_result_eta}{
  eta estimated by bootstrap method.
}
}

\value{
\item{alpha}{sorted alpha estimated by bootstrap method.}
\item{eta}{sorted eta estimated by bootstrap method.}
}

\author{
Linsui Deng
}


%% ~Make other sections like Warning with \section{Warning }{....} ~


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
                            
#test of EM_result_sort
EMsimulation_result_sort <- EM_result_sort(EMsimulation_result$alpha,
                                            EMsimulation_result$eta)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{arith}% use one of  RShowDoc("KEYWORDS")
