Ggenerate <-
function(eta,
                     X,
                     seed=0){
  if(is.null(eta) | is.null(X))
    stop("data is empty")
  #empty data isn't allowed
  samplesize <- dim(X)[1]
  classsize <- dim(eta)[2]
  G <- matrix(0,samplesize,classsize)
  
  if(classsize==1)
    stop("single class has no means")
  
  set.seed(seed)
  unirandom <- runif(samplesize,0,1)
  
  softmax_value <- softmax(eta,X)
  softmax_range <- t(apply(softmax_value,1,cumsum))
  #generate G
  Gtemp <- matrix(as.integer(softmax_range>=unirandom),samplesize,classsize)
  G[,1] <- Gtemp[,1]
  G[,2:classsize] <- Gtemp[,2:classsize] - Gtemp[,1:classsize-1]
  
  return(G)
}
