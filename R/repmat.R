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