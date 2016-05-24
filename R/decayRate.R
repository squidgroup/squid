# decayRate: apply autocorrelation to an environment using a decay rate.
#
# Args:
#   env:  the input environment without autocorrelation.
#   corr: numeric; correlation between two consecutive value.
#   Nb:   interger; dimension of the matrix.
#
# Returns:
#   the new environment wtih autocorrelation

decayRate <- function(env,corr, Nb){

  myMatrix <- matrix(0, nrow=Nb, ncol=Nb)
  
  if(corr == 0) corr <- 1e-10
  alpha     <- abs(log(corr)) 
  
  myMatrix  <- exp(-1*alpha*abs(col(myMatrix, as.factor = FALSE)-row(myMatrix, as.factor = FALSE)))

  newEnv    <-  myMatrix %*% env
  
  return(newEnv)

}