Ccompute <-
function(alphat,
                     sigmat,
                     etat,
                     X,
                     Z,
                     Y){
  samplesize <- dim(X)[1]
  classsize <- dim(alphat)[2]
  
  mu <- Z%*%alphat
  
  #calculate f(Y_i|G_i=k,X_i,Z_i;\gammat)
  fnorm_value <- fnorm(matrix(rep(Y,classsize),samplesize,classsize),mu,sigmat)
  
  #calculate P(G_i=k|X_i;\etat)
  softmax_value <- softmax(etat,X)
  
  #calculate f(Y_i,G_i=k|Z_i,X_i;\thetat)
  f_Y_G <- fnorm_value*softmax_value
  
  #calculate C_i,k=P(G_i=k|W_i;\thetat)
  C <- f_Y_G/apply(f_Y_G,1,sum) 
  
  return(C)
}
