# getIndMatrix: generate indviduals randome effects (intercepts and slopes)
#
# Args:
#   N:         internal list of simulation variables (related to simulation design). 
#   Mu:        mean value for the normal distributions.
#   VCov:      matrix of covariance/variance. Variances are on the matrix diagonal. 
#   Variables: list of the model general design.
#
# Returns:
#   matrix of individuals random effects in the intercepts and slopes.

getIndMatrix <- function(N, Mu, VCov, Variables){
  
  # Generate the ind matrix the multivariate normal distribution
  ind      <- matrix(0, N$NI*N$NP, nrow(VCov))
  ind[]    <- MASS::mvrnorm(N$NI*N$NP, rep(Mu,nrow(VCov)), VCov)
  
  # ind    = [NI, ind] 
  # raw    = number of individuals 
  # column = ind for each trait (ind0y, ind1y ... ind5y, ind0z, ind1z ... ind5z, ...)
  
  # Repeat every ind value (raw) NS times
  ind <- ind[rep(1:ifelse(!is.null(nrow(ind)),nrow(ind),1), each=N$NS),]
  ind <- reshapeMat(ind, Variables$nb.IS)
  
  return(ind)
}