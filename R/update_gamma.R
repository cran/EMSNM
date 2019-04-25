update_gamma <-
function(alphat,
                         sigmat,
                         etat,
                         X,
                         Z,
                         Y){
  #initialization
  sigma2 <- 0
  
  samplesize <- dim(X)[1]
  classsize <- dim(alphat)[2]
  alphasize <- dim(alphat)[1]
  
  C_value<- Ccompute(alphat,sigmat,etat,X,Z,Y)
  
  for(k in 1:classsize)
  {
    ZtPiZ <- weight_matrix(A=Z,W=C_value[,k],B=Z)
    ZtPiY <- weight_matrix(A=Z,W=C_value[,k],B=Y)
    
    #update\alpha_k
    alphat[,k] <- solve(ZtPiZ)%*%ZtPiY
    
    #update\sigma_k
    res_value <- Y-Z%*%alphat[,k] 
    sigma2 <- sigma2+weight_matrix(A=res_value,W=C_value[,k],B=res_value)
  }
  
  sigmat <- as.numeric(sqrt(sigma2/(samplesize-alphasize)))
  
  return(list(alpha=alphat,sigma=sigmat,eta=etat))#actually eta hasn't been updated
}
