EMalgorithm <-
function(X,
                        Y,
                        Z,
                        etat,#initial value
                        alphat,
                        sigmat,
                        classsize=2,
                        learning_rate=0.1,
                        regular_parameter_eta=0.001,
                        max_iteration=10000,
                        max_iteration_eta=10000,
                        compact_flag=FALSE,
                        C0=5,
                        C1=2,
                        C2=9){
  #initialization
  samplesize <- dim(X)[1]
  etasize <- dim(X)[2]
  alphasize <- dim(Z)[2]
  control_interation <- 0
  eta_diff <- 1
  alpha_diff <- 1
  sigma_diff <- 1
  
  if(is.null(etat))
    etat <- matrix(1,etasize,classsize)
  
  if(is.null(alphat))
    alphat <- matrix(1,alphasize,classsize)
  
  if(is.null(sigmat))
    sigmat <- 1
  
  etatemp <- etat
  alphatemp <- alphat
  sigmatemp <- sigmat
  
  while((eta_diff>1e-9 | alpha_diff>1e-9 | 
        sigma_diff>1e-9) & control_interation<max_iteration){
    etatemp <- etat
    alphatemp <- alphat
    sigmatemp <- sigmat
    control_interation <- control_interation+1
    
    #updata eta only when it's not nan
    thetaupdate_gamma <- update_gamma(alphat,sigmat,etat,X,Z,Y)
    if(is.nan(thetaupdate_gamma$sigma)){
        alphat <- alphatemp
    }
    else{
    alphat <- thetaupdate_gamma$alpha
    sigmat <- thetaupdate_gamma$sigma
    }
    
    #updateeta
    thetaupdate_eta <- update_eta(NULL,alphat,sigmat,
                                  etat,X,Y,Z,learning_rate,regular_parameter_eta,max_iteration_eta)
    #updata eta only when it satisfies some conditions
    if(compact_flag){
      eta_temp <- thetaupdate_eta$eta
      eta0 <- norm(matrix(eta_temp[1,1:(classsize-1)]),type='F')
      etaX <- norm(matrix(eta_temp[2:etasize,1:(classsize-1)]),type='F')
      if(!is.nan(eta0)&!is.nan(etaX)){
        if(eta0<=C0 & etaX>=C1 & etaX<=C2)
        {etat <- eta_temp}
      }
    }
    else
    {
      etat <- thetaupdate_eta$eta
    }
    
    eta_diff <- norm(etat-etatemp,type='F')
    alpha_diff <- norm(alphat-alphatemp,type='F')
    sigma_diff <- abs(sigmat-sigmatemp)
    
  }
  
  return(list(alpha=alphat,sigma=sigmat,eta=etat))
}
