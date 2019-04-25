EMsimulation <-
function(eta,
                         alpha,
                         sigma,
                         samplesize,
                         expriments,
                         compact_flag=FALSE,
                         C0=5,
                         C1=2,
                         C2=9){
  etasize <- dim(eta)[1]
  alphasize <- dim(alpha)[1]
  classsize <- dim(eta)[2]
  
  eta_est <- array(0,c(expriments,etasize,classsize)) 
  alpha_est <- array(0,c(expriments,alphasize,classsize)) 
  sigma_est <- array(0,c(expriments))
  Covmatrix_est <- array(0,c(expriments,2*alphasize+etasize+1,2*alphasize+etasize+1))
  
  for(i in 1:expriments)
  {
    set.seed(i)
    
    X <- matrix(c(matrix(1,samplesize),
                  rnorm(samplesize*(etasize-1))+1),samplesize,etasize)
    Z <- matrix(c(matrix(1,samplesize),rbinom(prob=1/2,size=1,n=samplesize),
                 rnorm(samplesize*(alphasize-2))+1),samplesize,alphasize)
    W <- Wgenerate(alpha=alpha,eta=eta,X=X,Z=Z,sigma=sigma)
    set.seed(i)
    
    eta_initial <- matrix((C1+0.001),etasize,classsize)
    eta_initial[,classsize] <- 0
    eta_initial[1,] <- 0
    alpha_initial<- matrix(rnorm(alphasize*classsize),alphasize,classsize)
    sigma_initial <- 1
    
    EMtheta <- EMalgorithm(X=W$X,Z=W$Z,Y=W$Y,classsize=classsize,
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
