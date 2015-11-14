### get_t : Generate the tx (time) matrices for each individual

get_t <- function(N, VCov){  
  
  L      <- N$NI*N$NT*N$NP
  
  # Allocate space to the matrix
  t0     <- array(data=NA,dim=c(L, N$NS), dimnames=c("Individuals","Records"))
  t0[,1] <- rep(0,L)
  
  M      <- sapply(2:N$NS, function(x){ t0[,x] <<- t0[,x-1] + t(mvrnorm(N$NI*N$NP, rep(0,N$NT), VCov)) })
  
  t0     <- as.vector(t(t0))
  
  return(t0)

}
