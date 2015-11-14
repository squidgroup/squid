### Generate the xyij matrix

get_x_Matrix_linear <- function(N, Mu, V){  
  
  x <- 1:N$NS
  
  # Generate values from a normal distribution
  myMatrix <- rep((V/1000)*x - 0.25, N$NI*N$NT*N$NP)
  
  return(myMatrix)
  
}