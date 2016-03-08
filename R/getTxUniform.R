#' getTxUniform
#'
#' @description          generate the sampling time for each individual, each trait and each population
#'
#' @return               matrix of sampling time
#'
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