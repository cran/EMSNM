\name{EM_parameter_sd}
\alias{EM_parameter_sd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Bootstrap Parameter Inference
}
\description{
Estimating the parameters and their stand error through the sorted parameters estimated by bootstrap method.
}
\usage{
EM_parameter_sd(EMsimulation_sort_alpha, EMsimulation_sort_eta, EMsimulation_sort_sigma)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{EMsimulation_sort_alpha}{
  sorted alpha estimated by bootstrap method.
}
  \item{EMsimulation_sort_eta}{
  sorted eta estimated by bootstrap method.
}
  \item{EMsimulation_sort_sigma}{
  sorted sigma estimated by bootstrap method.
}
}


\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
\item{sigma}{the estimation of sigma}
\item{sigma_sd}{the estimation of standard error of sigma}
\item{alpha}{the estimation of alpha}
\item{alpha_sd}{the estimation of standard error of alpha}
\item{eta}{the estimation of eta}
\item{eta_sd}{the estimation of standard error of eta}
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

EMsimulation_result <- EMsimulation(eta=etatest,alpha=alphatest,sigma=sigmatest,
                            samplesize=samplesize,expriments=expriments,
                            compact_flag=TRUE,C0=5,C1=0.5,C2=5)
index <- which(EMsimulation_result$sigma<0.8)
EMsimulation_result_sort <- EM_result_sort(EMsimulation_result$alpha[index,,],
                                            EMsimulation_result$eta[index,,])
#test of EM_parameter_sd
EM_parameter <- EM_parameter_sd(EMsimulation_result_sort$alpha,
                                EMsimulation_result_sort$eta,
                                EMsimulation_result$sigma[index])
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{htest}% use one of  RShowDoc("KEYWORDS")
\keyword{nonparametric}