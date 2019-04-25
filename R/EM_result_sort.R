EM_result_sort <-
function(EMsimulation_result_alpha,EMsimulation_result_eta)
{
  classsize <- dim(EMsimulation_result_alpha)[3]
  alphasize <- dim(EMsimulation_result_alpha)[2]
  etasize <- dim(EMsimulation_result_eta)[2]
  samplesize <- dim(EMsimulation_result_alpha)[1]
  for(i in 1:samplesize)
{
  for(j in 1:classsize)
  {
    for(k in j:classsize)
    {
      if(EMsimulation_result_alpha[i,1,j]<EMsimulation_result_alpha[i,1,k])
      {
        alpha_temp <- EMsimulation_result_alpha[i,,j]
        EMsimulation_result_alpha[i,,j] <- EMsimulation_result_alpha[i,,k]
        EMsimulation_result_alpha[i,,k] <- alpha_temp
        
        eta_temp <- EMsimulation_result_eta[i,,j]
        EMsimulation_result_eta[i,,j] <- EMsimulation_result_eta[i,,k]
        EMsimulation_result_eta[i,,k] <- eta_temp
      }
    }
  }
}
  EMsimulation_result_eta_final <- matrix(EMsimulation_result_eta[,,classsize],
                                          samplesize*etasize*classsize)
  dim(EMsimulation_result_eta_final) <- c(samplesize,etasize,classsize)
  EMsimulation_result_eta <- EMsimulation_result_eta-EMsimulation_result_eta_final
  return(list(alpha=EMsimulation_result_alpha,eta=EMsimulation_result_eta))
}
