EMbootstrap <-
function(X,
                        Y,
                        Z,
                        samplesize,
                        boots_samplesize,
                        boots_expriments,
                        classsize=2,
                        learning_rate=0.1,
                        regular_parameter_eta=0.001,
                        max_iteration=10000,
                        max_iteration_eta=10000,
                        compact_flag=FALSE,
                        C0=5,
                        C1=2,
                        C2=9){
  etasize <- dim(X)[2]
  alphasize <- dim(Z)[2]
  
  eta_est <- array(0,c(boots_expriments,etasize,classsize)) 
  alpha_est <- array(0,c(boots_expriments,alphasize,classsize)) 
  sigma_est <- array(0,c(boots_expriments))
  
  for(i in 1:boots_expriments)
  {
    set.seed(i)
    print(i)
    index <- sample(1:samplesize, size = boots_samplesize, replace = TRUE)
    Wtest <- list(X=X[index,], Y=matrix(Y[index,],boots_samplesize,1),
                  Z=Z[index,])
    
    #initializaiton of parameters
    eta_initial <- matrix(c(rnorm(etasize*(classsize-1)),matrix(0,etasize)),etasize,classsize)
    alpha_initial<- matrix(rnorm(alphasize*classsize),alphasize,classsize)
    sigma_initial <- 1
  
    EMtheta <- EMalgorithm(X=Wtest$X,Z=Wtest$Z,Y=Wtest$Y,classsize=classsize,
                           etat=eta_initial,alphat=alpha_initial,sigmat=sigma_initial,
                           learning_rate=0.01,regular_parameter_eta=0.001,
                           max_iteration=1000,max_iteration_eta=10000,
                           compact_flag,C0,C1,C2)
    
    eta_est[i,,] <- EMtheta$eta
    alpha_est[i,,] <- EMtheta$alpha
    sigma_est[i] <- EMtheta$sigma
  }
  
  return(list(eta=eta_est,alpha=alpha_est,sigma=sigma_est))
 }
