weight_matrix <-
function(A,
                          W,
                          B){
  if(dim(A)[1]!=dim(B)[1])#prevent different dimension
    stop("the dimensions are different")
  else
    samplesize <- dim(A)[1]
  
  W_matrix <- diag(as.vector(W))
  weight_matrix <- t(A)%*%W_matrix%*%B
  
  return(weight_matrix)
}
