# repmat: repeat a matrix object.
#
# Args:
#   X:    The matrix object to repeat. 
#   m:    integer; number of times to repeat in the vertical direction.
#   n:    integer; number of times to repeat in the horizontal direction.
#
# Returns:
#   matrix

repmat <- function(X,m,n){
  
  mx <- dim(X)[1]
  nx <- dim(X)[2]
  
  return(matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T))
}


# reshapeMat: reshape a matrix object.
#
# Args:
#   X:    The matrix object to reshape 
#   step: number of columns.
#
# Returns:
#   matrix

reshapeMat <- function(mat, step){
  
  newMat <- NULL
  itr    <- ncol(mat)/step
  
  myFun    <- function(i){
    max    <- step*i
    min    <- max-(step-1)
    newMat <<- rbind(newMat, mat[,min:max])
  }
  M        <- sapply(1:itr, myFun)
  
  return(newMat)
}


# decayRate: apply autocorrelation to an environment using a decay rate.
#
# Args:
#   env:  the input environment without autocorrelation.
#   corr: numeric; correlation between two consecutive value.
#   Nb:   integer; dimension of the matrix.
#
# Returns:
#   the new environment wtih autocorrelation

decayRate <- function(env, corr, Nb){
  
  myMatrix <- abs(corr) ^ as.matrix(dist(1:Nb))
  newEnv   <-  myMatrix %*% env
  
  return(newEnv)
  
}