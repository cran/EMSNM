\name{weight_matrix}
\alias{weight_matrix}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weighted Inner Product
}
\description{
Calculate the weighted inner product of A and B with the weight W. This result is useful in Logistic Regression.
}
\usage{
weight_matrix(A, W, B)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{A}{
  the left matrix
}
  \item{W}{
  the weight 
}
  \item{B}{
  the right matrix
}
}

\value{
the wighted inner product, noticing it can be vector when A or B is 2 dimension. 
}

\author{
Linsui Deng
}


\examples{
#data generation
A <- matrix(rnorm(200),100,2)
W <- rnorm(100)
B <- matrix(rnorm(100),100,1)
weighted <- weight_matrix(A,W,B)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{algebra}% use one of  RShowDoc("KEYWORDS")
\keyword{array}
