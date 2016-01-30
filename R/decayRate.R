decayRate <- function(env,corr, Nb){

  myMatrix <- matrix(0, nrow=Nb, ncol=Nb)
  
  if(corr == 0) corr <- 1e-10
  alpha     <- abs(log(corr)) 
  
  myMatrix  <- exp(-1*alpha*abs(col(myMatrix, as.factor = FALSE)-row(myMatrix, as.factor = FALSE)))

  newEnv    <-  myMatrix %*% env
  
  return(newEnv)

}