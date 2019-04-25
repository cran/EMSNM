fnorm <-
function(Y,
                  mu,
                  sigma){
  classsize <- dim(mu)[2]
  samplesize <- dim(Y)[1]
  fnorm_value <- exp(-(matrix(Y,samplesize,classsize)-mu)^2/(2*sigma^2))/(sqrt(2*pi*sigma^2))
  
  return(fnorm_value)
}
