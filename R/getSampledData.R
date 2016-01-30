#' getSampledData
#'
#' @description          extract sampling data from the real data
#'
#' @return               data.frame of sampled model data
#'
getSampledData <- function(N, Time, full_Data){

    # get number of records for each individual
    N$NRI  <- getNRI(N, Time) 
    
    # update the dimension of the matrices with the maximum record number
    N$NR   <- max(N$NRI)
    
    # get the sampling time for each individual, each trait and each population
    Tx     <- N$NS * (c(1:(N$NI*N$NT*N$NP)) - 1) + getTxUniform(N, Time)
    Tx     <- as.numeric(na.omit(as.vector(t(Tx))))

    # get phenotype of the sampling individuals
    sampled_Data <- full_Data[Tx,]

  return(sampled_Data)
}