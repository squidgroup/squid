### Generate the xyij matrix

get_x_Matrix_cyclic <- function(N, Mu, V){  
  
  require("MASS")
  
  t <- 1:N$NS
  
  # Generate values from a normal distribution
  myMatrix <- rep(V*sin(t/100), N$NI*N$NT*N$NP)
  
  return(myMatrix)
  
}
