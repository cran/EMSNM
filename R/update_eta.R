update_eta <-
function(fun,
                      alphat,
                      sigmat,
                      etat,
                      X,
                      Y,
                      Z,
                      learning_rate_eta=0.001,
                      regular_parameter_eta=0.001,
                      max_iteration_eta=10000){
  etatemp<- etat
  etasize <- dim(etat)[1]

  samplesize <- dim(X)[1]
  classsize <- dim(alphat)[2]
  C_value <- Ccompute(alphat,sigmat,etat,X,Z,Y)
  
  adapt_C_value_temp <- pmax(matrix(0.05/(classsize-1),samplesize,classsize),
                            pmin(matrix(0.95,samplesize,classsize),C_value))
  adapt_C_value_temp <- C_value
  Reg_value <- matrix(log(adapt_C_value_temp/(1-adapt_C_value_temp)),samplesize,classsize)
  Xtemp <- as.matrix(X)
  exp_excpt_value <- matrix(apply(exp(Xtemp%*%etat),1,sum),samplesize,classsize)-
                      exp(Xtemp%*%etat)
  Reg_value <- matrix(log(adapt_C_value_temp*exp_excpt_value/(1-adapt_C_value_temp)),samplesize,classsize)
  
  
  etatemp_temp <- solve(t(Xtemp)%*%(Xtemp))%*%(t(Xtemp)%*%Reg_value)
    
  etatemp[,1:(classsize-1)] <- etatemp_temp[,1:(classsize-1)]-
                              matrix(etatemp_temp[,classsize],etasize,classsize-1)
  return(list(alpha=alphat,sigma=sigmat,eta=etatemp))
}
