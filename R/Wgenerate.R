Wgenerate <-
function(alpha,
                     sigma=1,
                     eta,
                     samplesize=0,
                     X,
                     Z,
                     seed1=0,
                     seed2=0){
  if(samplesize==0&is.null(X)&is.null(Z))
    stop("data is empty")
  
  if(is.null(alpha)|is.null(eta))
    stop("coeffiencts can't be empty")
  
  #the size of sample
  if(samplesize==0){
    if(!is.null(X))
       samplesize <- dim(X)[1]
    else
       samplesize <- dim(Z)[1]
  }
  
  #class size
  classsize <- dim(alpha)[2]
  if(classsize!=dim(eta)[2])
    stop("the class size is vague")
  
  alphasize <- dim(alpha)[1]
  etasize <- dim(eta)[1]
  
  #generate X and Z if they aren't exists
  set.seed(seed1)
  if(is.null(X))
    X <- matrix(rnorm(samplesize*alphasize,0,1),samplesize,alphasize)
  if(is.null(Z))
    Z <- matrix(rnorm(samplesize*etasize,0,1),samplesize,etasize)
  
  #calculate the mean value of each class
  mu <- Z%*%alpha#dim=samplesize*alphasize
  
  #simulation the distribution
  Ytemp <- matrix(rnorm(samplesize*classsize,0,1),samplesize,classsize)
  
  Ytemp <- Ytemp*sigma+mu
  
  #generate G and Y
  G <- Ggenerate(eta,X,seed2)
  Y <- matrix(apply(Ytemp*G,1,sum),samplesize,1)
  
  return(list(X=X,Z=Z,Y=Y,G=G))
}
