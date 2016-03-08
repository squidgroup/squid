#' getNRI
#'
#' @description          generate number of records for each individual
#'
#' @return               vector of number of records per individual and trait
#'
getNRI <- function(N, Time){
  
  nbSample.NT   <- N$NI*N$NP*N$NT
  nbSample      <- N$NI*N$NP
  
  # Generate record number for each individual with a poisson distribution 
  # with a lamda equal to NR 
  if(!Time$NR_ind){
    if(!Time$NR_trait){
      # Difference in records number among individuals and traits
      NRI <- as.integer(rpois(nbSample.NT, N$NR)) 
    }else{
      # Difference in records number among individuals and  same among traits
      NRI <- as.integer(rpois(nbSample, N$NR))
      NRI <- rep(NRI, N$NT)
    }
  }else{
    if(!Time$NR_trait){
      # Difference in records number among Traits and  same among individuals
      NRI <- NULL
      myFun  <- function(i){
        Temp <-  as.integer(rpois(N$NP, N$NR))
        NRI  <<- c(NRI, rep(Temp, each=N$NI))
      }
      M      <- sapply(1:N$NT, myFun)      
    }else{
      # Same number of records among individuals and traits
      if(!Time$NR_Fixe){ # if NR is fixed (same for all populations)
        NRI <- rep(rep(as.integer(rpois(N$NP, N$NR)),each=N$NI),N$NT)
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