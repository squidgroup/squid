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