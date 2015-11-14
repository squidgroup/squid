# get_VCov : Build the (co)vairante matrix 
# The covariance is calculated as function of variance and regression coefficiant 

get_VCov_Matrix <- function(VCov){

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