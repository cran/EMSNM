EM_parameter_sd <-
function(EMsimulation_sort_alpha,
                            EMsimulation_sort_eta,
                            EMsimulation_sort_sigma)
{
  classsize <- dim(EMsimulation_sort_alpha)[3]
  alphasize <- dim(EMsimulation_sort_alpha)[2]
  etasize <- dim(EMsimulation_sort_eta)[2]
  samplesize <- dim(EMsimulation_sort_alpha)[1]
  
  alpha <- matrix(0,alphasize,classsize)
  alpha_sd <- matrix(0,alphasize,classsize)
  
  eta <- matrix(0,etasize,classsize)
  eta_sd <- matrix(0,etasize,classsize)
  
  for(j in 1:classsize)
  {
    for(i in 1:alphasize)
    {
      alpha[i,j] <- mean(EMsimulation_sort_alpha[,i,j])
      alpha_sd[i,j] <- sd(EMsimulation_sort_alpha[,i,j])
    }
    for(i in 1:etasize)
    {
      eta[i,j] <- mean(EMsimulation_sort_eta[,i,j])
      eta_sd[i,j] <- sd(EMsimulation_sort_eta[,i,j])
    }
  }
  sigma <- mean(EMsimulation_sort_sigma)
  sigma_sd <- sd(EMsimulation_sort_sigma)
  
  return(list(sigma=sigma,sigma_sd=sigma_sd,alpha=alpha,alpha_sd=alpha_sd,
              eta=eta,eta_sd=eta_sd))
}
