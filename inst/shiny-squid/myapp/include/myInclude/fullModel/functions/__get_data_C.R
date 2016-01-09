# Generates the data for each phenotype trait

get_data_C <- function(Mu, N, B, r, V, Time, Variables, Env){
  
    #############################################  
    
    if(sum(diag(V$Vind)) != 0){
      # Random intercept and slope (Co)Variance matrix
#         VCov       <- cppGetVCovMatrix(V$Vind) 
        VCov       <- get_VCov_Matrix(V$Vind)
        
      ### Generate random intercept and slope for each individual and trait
      ind          <- get_Ind_Matrix(N, Mu, VCov, Variables)
#       ind          <- cppGetIndMatrix(N$NI, N$NP, rep(Mu,nrow(VCov)), VCov, Variables$nb.IS)

    }else{
      ind          <- matrix(0,  N$NI*N$NS*N$NP*N$NT, Variables$nb.IS)  
    }   
    
    ##############################################  
    # Environement value 
    X                <- matrix(0,  N$NI*N$NS*N$NP, Variables$nb.IS)    
    X[,Variables$B0] <- 1 # Intercept (slope is by default = 1 ) 

    ### Generate environments
    if(Env$X1$state){X[,Variables$X1] <- get_environment(Env$X1, N, FALSE)}else{ X[,Variables$X1] <- 0 }
    if(Env$X2$state){X[,Variables$X2] <- get_environment(Env$X2, N, FALSE)}else{ X[,Variables$X2] <- 0 }
    
    # Interaction 
    if(Env$Interaction) X[,Variables$X1X2] <- X[,Variables$X1]*X[,Variables$X2]

    # Add environment for all traits
    X <- repmat(X,N$NT,1)
    
    ############################################## 
    # Higher-level grouping variance (h)  
    if(V$Vk == 0){
      K   <- vector(N$NI*N$NT*N$NP*N$NS,mode = "double")
    }else{
      K   <- rep(rep(rnorm(N$NK*N$NT*N$NP, Mu, sqrt(V$Vk)), each=N$NI/N$NK), each=N$NS)
    }
      
    ############################################## 
    # Measurement error variance (me)  
    ME           <- rnorm(N$NI*N$NT*N$NP*N$NS, Mu, sqrt(V$Vme))
    
    ############################################## 
    
    # Phenotypic equation
    Phenotype    <-  rowSums((B + ind) * X) + K + ME
    
    ############################################## 
    
    Individual   <- rep(rep(1:(N$NI*N$NP), each=N$NS), N$NT)
    
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
    
    data_C       <- data.frame("Replicate" = Population,                                
                               Individual,
                               Individual_Trait,
                               Trait, 
                               "Time" = time,
                               Phenotype,
                               "B0"  = B[,Variables$B0],
                               "Be1" = B[,Variables$X1],
                               "Be2" = B[,Variables$X2],
                               "Be12" = B[,Variables$X1X2],
                               "J0"  = ind[,Variables$B0],
                               "Je1" = ind[,Variables$X1],
                               "Je2" = ind[,Variables$X2],
                               "Je12" = ind[,Variables$X1X2],
                               "X1"  = X[,Variables$X1],
                               "X2"  = X[,Variables$X2],
                               "X1X2"= X[,Variables$X1X2],
                               "G" = K,
                               ME
                               )

  return(data_C)
}