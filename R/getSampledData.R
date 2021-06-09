# getNRI: generate number of records for each individual
#
# Args:
#   N:         internal list of simulation variables (related to simulation design). 
#   Time:         internal list of simulation variables (related to simulation timing). 
#
# Returns:
#   vector of number of records per individual and trait.

getNRI <- function(N, Time){
  
  nbSample.NT   <- N$NI*N$NP*N$NT
  nbSample      <- N$NI*N$NP
  
  # Generate record number for each individual with a poisson distribution 
  # with a lamda equal to NR 
  if(!Time$NR_ind){
    if(!Time$NR_trait){
      # Difference in records number among individuals and traits
      NRI <- as.integer(stats::rpois(nbSample.NT, N$NR)) 
    }else{
      # Difference in records number among individuals and  same among traits
      NRI <- as.integer(stats::rpois(nbSample, N$NR))
      NRI <- rep(NRI, N$NT)
    }
  }else{
    if(!Time$NR_trait){
      # Difference in records number among Traits and  same among individuals
      NRI <- NULL
      myFun  <- function(i){
        Temp <-  as.integer(stats::rpois(N$NP, N$NR))
        NRI  <<- c(NRI, rep(Temp, each=N$NI))
      }
      M      <- sapply(1:N$NT, myFun)      
    }else{
      # Same number of records among individuals and traits
      if(!Time$NR_Fixe){ # if NR is fixed (same for all populations)
        NRI <- rep(rep(as.integer(stats::rpois(N$NP, N$NR)),each=N$NI),N$NT)
      }else{
        NRI <- rep(N$NR, N$NI*N$NT*N$NP)
      }
    }
  }
  
  # Adjust if number of records is outside the sampling limits
  NRI[NRI == 0] <- 1
  NRI[NRI  > Time$TsampI] <- Time$TsampI 
  
  return(NRI)
}


# getTxUniform: generate the sampling time for each individual, each trait and each population
#
# Args:
#   N:            internal list of simulation variables (related to simulation design). 
#   Time:         internal list of simulation variables (related to simulation timing). 
#
# Returns:
#   matrix of sampling time

getTxUniform <- function(N, Time){  
  
  if(Time$Tsamp > 1){
    
    Mu          <- Time$Tsamp/2 # mean total sampling time lenght 
    diff        <- (Time$Tsamp - Time$TsampI)/2
    MuInterval  <- c((Mu-diff):(Mu+diff))
    
    if(!Time$ST_ind){      
      if(!Time$ST_trait){
        
        # Difference in sampling time among individuals and traits
        # Generate individual mean time sampling
        if(diff!=0){ sampMean <- sample(MuInterval, length(N$NRI), replace=T)
        }else{sampMean <- rep(MuInterval, length(N$NRI))}
        
        sampMin <- sampMean - (Time$TsampI/2)
        sampMin[sampMin == 0] <- 1
        sampMax <- sampMean + (Time$TsampI/2)
        
        # Generate Time sampling for each individual 
        Tx     <- matrix(NA, nrow = length(N$NRI), ncol = N$NR)        
        myFun  <- function(x){ Tx[x,1:N$NRI[x]] <<- sort(sample(sampMin[x]:sampMax[x],N$NRI[x], replace=F))}
        M      <- sapply(1:length(N$NRI), myFun)
        
      }else{
        
        # Difference in sampling time among individuals and same among traits
        # Generate individual mean time sampling
        if(diff!=0){
          sampMean <- sample(MuInterval, length(N$NRI)/N$NT, replace=T)
        }else{sampMean <- rep(MuInterval, length(N$NRI)/N$NT)}
        
        sampMin <- sampMean - (Time$TsampI/2)
        sampMin[sampMin == 0] <- 1
        sampMax <- sampMean + (Time$TsampI/2)
        
        # Generate Time sampling for each individual 
        Tx     <- matrix(NA, nrow = length(sampMean), ncol = N$NR)        
        myFun  <- function(x){ Tx[x,1:N$NRI[x]] <<- sort(sample(sampMin[x]:sampMax[x],N$NRI[x], replace=F))}
        M      <- sapply(1:length(sampMean), myFun)
        Tx     <- Tx[rep(1:nrow(Tx), N$NT), ]
      }
      
    }else{
      
      if(!Time$ST_trait){
        
        # Difference in sampling time among Traits and same among individuals
        
        if(diff!=0){
          sampMean <- sample(MuInterval, N$NT*N$NP, replace=T)
        }else{sampMean <- rep(MuInterval, N$NT*N$NP)}
        
        sampMin <- sampMean - (Time$TsampI/2)
        sampMin[sampMin == 0] <- 1
        sampMax <- sampMean + (Time$TsampI/2)
        
        coord <- seq(from=1, to=length(N$NRI), by=N$NI)
        
        Tx     <- matrix(NA, nrow = length(coord), ncol = N$NR)        
        myFun  <- function(x){ Tx[x,1:N$NRI[coord[x]]] <<- sort(sample(sampMin[x]:sampMax[x],N$NRI[coord[x]], replace=F))}
        M      <- sapply(1:length(coord), myFun)
        Tx     <- Tx[rep(1:nrow(Tx), each=N$NI), ]                
        
      }else{
        
        # Same time of sampling among individuals and traits
        if(diff!=0){sampMean <- sample(MuInterval, N$NP, replace=T)
        }else{sampMean <- rep(MuInterval, N$NP)}
        
        sampMin <- sampMean - (Time$TsampI/2)
        sampMin[sampMin == 0] <- 1
        sampMax <- sampMean + (Time$TsampI/2)
        
        coord <- seq(from=1, to=N$NI*N$NP, by=N$NI)
        
        Tx     <- matrix(NA, nrow = length(coord), ncol = N$NR)        
        myFun  <- function(x){ Tx[x,1:N$NRI[coord[x]]] <<- sort(sample(sampMin[x]:sampMax[x],N$NRI[coord[x]], replace=F))}
        M      <- sapply(1:length(coord), myFun)
        Tx     <- Tx[ rep(rep( 1:nrow(Tx), each=N$NI), N$NT), ]        
      }
    }
  }else{
    # Generate Time sampling for each individual 
    Tx     <- matrix(1, nrow = length(N$NRI), ncol = 1)
  }
  
  Tx <- (Tx + Time$Ts) - 1
  
  return(Tx)
  
}


# getSampledData: extract sampling data from the real data
#
# Args:
#   N:            internal list of simulation variables (related to simulation design). 
#   Time:         internal list of simulation variables (related to simulation timing). 
#   full_Data:    data.frame; the full data generated.
#
# Returns:
#   data.frame of sampled model data

getSampledData <- function(N, Time, full_Data){

    # get number of records for each individual
    N$NRI  <- getNRI(N, Time) 
    
    # update the dimension of the matrices with the maximum record number
    N$NR   <- max(N$NRI)
    
    # get the sampling time for each individual, each trait and each population
    Tx     <- N$NS * (c(1:(N$NI*N$NT*N$NP)) - 1) + getTxUniform(N, Time)
    Tx     <- as.numeric(stats::na.omit(as.vector(t(Tx))))

    # get phenotype of the sampling individuals
    sampled_Data <- full_Data[Tx,]

  return(sampled_Data)
}