# Cor2CovMatrix
#
# description          Transform a correlation/variance matrix to a covariance/variance matrix. 
#
# param  CorCov        matrix of correlation/variance. Variances are on the matrix diagonal.
#
# return               matrix of covariance/variance.
Cor2CovMatrix <- function(CorCov){

  VCov   <- CorCov
  Vdim   <- length(diag(VCov))
  
  if(Vdim != 1){
    k <- 1
    for(i in 1:(Vdim-1)){
      for(j in 1:i){
        VCov[i+1,j] <- VCov[i+1,j]*sqrt(VCov[j,j]*VCov[i+1,i+1])
        VCov[j,i+1] <- VCov[i+1,j]
        k <- k + 1
      }
    }
  }
    
  return(VCov)
}