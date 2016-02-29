#' get_full_data
#'
#' @description          generate data for each phenotype trait
#'
#' @return               data.frame full model data
#'
getFullData <- function(Mu, N, B, r, V, Time, variables, environments){
  
    #############################################  
    
    if(sum(diag(V$Vind)) != 0){
      # Random intercept and slope (Co)Variance matrix
#         VCov       <- cppGetVCovMatrix(V$Vind) 
        VCov       <- SQUID::Cor2CovMatrix(V$Vind)
        
      ### Generate random intercept and slope for each individual and trait
      ind          <- getIndMatrix(N, Mu, VCov, variables)
#       ind          <- cppGetIndMatrix(N$NI, N$NP, rep(Mu,nrow(VCov)), VCov, variables$nb.IS)

    }else{
      ind          <- matrix(0,  N$NI*N$NS*N$NP*N$NT, variables$nb.IS)  
    }   
    
    ##############################################  
    # Environement value 
    X                <- matrix(0,  N$NI*N$NS*N$NP, variables$nb.IS)    
    X[,variables$B0] <- 1 # Intercept (slope is by default = 1 ) 

    ### Generate environments
    if(environments$X1$state){
      X[,variables$X1] <- getEnvironment(environments$X1, N, FALSE)
    }else{ 
      X[,variables$X1] <- 0
    }
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
    # Higher-level grouping variance (h)  
    if(V$VG == 0){
      G   <- vector(N$NI*N$NT*N$NP*N$NS, mode = "double")
    }else{
      G   <- rep(rep(rnorm(N$NG*N$NT*N$NP, Mu, sqrt(V$VG)), each=N$NI/N$NG), each=N$NS)
    }
      
    ############################################## 
    # Measurement error variance (me)  
    e           <- rnorm(N$NI*N$NT*N$NP*N$NS, Mu, sqrt(V$Ve))
    
    ############################################## 
    # Phenotypic equation
    Phenotype    <-  rowSums((B + ind) * X) + G + e
    
    ############################################## 
    
    Individual       <- rep(rep(1:(N$NI*N$NP), each=N$NS), N$NT)
    
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