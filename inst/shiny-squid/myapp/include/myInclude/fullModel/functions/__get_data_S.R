# Get sampling data from the real data

get_data_S <- function(N, Time, data_C){

    # get number of records for each individual
    N$NRI  <- get_NRI(N, Time) 
    
    # update the dimension of the matrices with the maximum record number
    N$NR   <- max(N$NRI)
    
    # get the sampling time for each individual, each trait and each population
    Tx     <- N$NS * (c(1:(N$NI*N$NT*N$NP)) - 1) + get_Tx_uniform(N, Time)
    Tx     <- as.numeric(na.omit(as.vector(t(Tx))))

    # get phenotype of the sampling individuals
    data_S <- data_C[Tx,]

  return(data_S)
}