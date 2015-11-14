get_x_Matrix_auto <- function(N, Mu, V){
  
  require("MASS")
  
  SD       <- sqrt(V/N$NS)
  myMatrix <- rep(NA, N$NS)
  
  for(i in 1:N$NS){
    
    if(i == 1) Mu <- 0 else Mu <-  myMatrix[i-1]
    
    myMatrix[i] <- rnorm(1, Mu, SD)
    
  }
  
  myMatrix <- rep(myMatrix, N$NI*N$NT*N$NP)
  
  return(myMatrix)
  
}
