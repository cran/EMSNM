softmax <-
function(eta,
                    X){
  Xmatrix <- as.matrix(X)
  exp_value <- exp(Xmatrix%*%eta)
  
  softmax_value <- exp_value/apply(exp_value,1,sum)
  
  return(softmax_value)
}
