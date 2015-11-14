### Generate the xyij matrix

get_x_Matrix_ran <- function(N, Mu, V){  
  
  require("MASS")
  
  # Generate values from a normal distribution
  myMatrix <- rep(rnorm(N$NS, Mu, sqrt(V)), N$NI*N$NT*N$NP)
  
  return(myMatrix)
  
}

