# Cor2CovMatrix: Transform a correlation/variance matrix to a covariance/variance matrix.
#
# Args:
#   VCor:      matrix of correlation/variance. Variances are on the matrix diagonal.
#
# Returns:
#   matrix of covariance/variance
#

Cor2CovMatrix <- function(VCor){
  
  VCov   <- VCor
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



# getFullData: generates full data for each phenotype trait
#
# Args:
#   Mu:  mean value for the normal distributions.
#   N:            internal list of simulation variables (related to simulation design). 
#   B:            matrix of the mean population values (intercept and slopes).
#   V:            list of the variances used for the simulation.
#   Time:         internal list of simulation variables (related to simulation timing). 
#   variables:    list of the model general design.
#   environments: list of the environments info.
#
# Returns:
#   data.frame of the full model data

getFullData <- function(Mu, N, B, V, Time, variables, environments){
  
    #############################################  
    
    if(sum(diag(V$Vind)) != 0){
      # Random intercept and slope (Co)Variance matrix
#         VCov       <- cppGetVCovMatrix(V$Vind) 
        VCov       <- Cor2CovMatrix(V$Vind)
      ### Generate random intercept and slope for each individual and trait
      ind          <- getIndMatrix(N, Mu, VCov, variables)
#       ind          <- cppGetIndMatrix(N$NI, N$NP, rep(Mu,nrow(VCov)), VCov, variables$nb.IS)

    }else{
      ind          <- matrix(0,  N$NI*N$NS*N$NP*N$NT, variables$nb.IS)  
    }   
	
    #######################################################################################
    # Create environmental effect values 
    X                <- matrix(0,  N$NI*N$NS*N$NP, variables$nb.IS)    
    X[,variables$B0] <- 1 # Intercept (slope is by default = 1 ) 
    
    ### Generate environments
    # X1
    if(environments$X1$state){
      X[,variables$X1] <- getEnvironment(environments$X1, N, FALSE)
    }else{ 
      X[,variables$X1] <- 0
    }
    # X2
    if(environments$X2$state){
      X[,variables$X2] <- getEnvironment(environments$X2, N, FALSE)
    }else{
      X[,variables$X2] <- 0
    }

    # Interaction 
    if(environments$Interaction) X[,variables$X1X2] <- X[,variables$X1]*X[,variables$X2]
    
    # Add environment for all traits
    X <- repmat(X,N$NT,1)
    
    ############################################## 
    # Higher-level grouping (Co)variance (h)  
    if(sum(diag(V$VG)) != 0){
     	VCov_G <- Cor2CovMatrix(V$VG)
    	G      <- rep(rep(as.vector(MASS::mvrnorm(N$NG*N$NP, rep(Mu,N$NT), VCov_G)), each=N$NI/N$NG), each=N$NS)
    }else{
    	G      <- vector(N$NI*N$NT*N$NP*N$NS, mode = "double")
    }

    Group <- as.factor(rep(rep(1:N$NG, each=N$NI/N$NG), each=N$NS))
    
    ############################################## 
    # Residual (Co)Variance matrix
    if(sum(diag(V$Ve)) != 0){
    	VCov_e <- Cor2CovMatrix(V$Ve)
    	### Generate residuals 
    	e      <- as.vector(MASS::mvrnorm(N$NI*N$NS*N$NP, rep(Mu,N$NT), VCov_e))
    }else{
    	e      <- rep(0,  N$NI*N$NS*N$NP*N$NT)  
    }
      
    ############################################## 
    # Phenotypic equation
    Phenotype   <-  base::rowSums((B + ind) * X) + G + e
    
    ############################################## 
    
    Individual       <- rep(rep(rep(1:N$NI, each=N$NS), N$NP), N$NT)
    
    Individual_Trait <- vector(mode="integer", N$NI*N$NP*N$NT)
    
    myFun  <- function(i){
     mySeq <- c(seq(from = i, to = (N$NI*N$NP*N$NT), by = N$NT))
     x <- (i-1) * N$NI*N$NP + 1
     y <- i * N$NI*N$NP
     Individual_Trait[x:y] <<- mySeq
    }
    M      <- sapply(1:N$NT, myFun)
    Individual_Trait <- as.factor(rep(Individual_Trait, each=N$NS))
    
    Population   <- as.factor(rep(rep(1:N$NP, each=N$NI*N$NS),N$NT))
    Trait        <- as.factor(rep(1:N$NT, each=N$NS*N$NI*N$NP))
    time         <- rep(1:N$NS, N$NI*N$NP*N$NT)
    
    full_Data    <- data.frame("Replicate"        = Population,
                               "Individual"       = Individual,
                               "Group"            = Group,
                               "Individual_Trait" = Individual_Trait,
                               "Trait"            = Trait,
                               "Time"             = time,
                               "Phenotype"        = Phenotype,
                               "B0"               = B[,variables$B0],
                               "B1"               = B[,variables$X1],
                               "B2"               = B[,variables$X2],
                               "B12"              = B[,variables$X1X2],
                               "I"                = ind[,variables$B0],
                               "S1"               = ind[,variables$X1],
                               "S2"               = ind[,variables$X2],
                               "S12"              = ind[,variables$X1X2],
                               "X1"               = X[,variables$X1],
                               "X2"               = X[,variables$X2],
                               "X1X2"             = X[,variables$X1X2],
                               "G"                = G,
                               "e"                = e)

  return(full_Data)
}