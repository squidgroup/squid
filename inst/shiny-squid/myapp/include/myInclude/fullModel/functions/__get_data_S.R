# get_data_S : Get the sampling data from the real data

get_data_S <- function(N, Time, data_C, session, progress){
  
#   data_S <- withProgress(session, {
#     
#     setProgress(message = 'Sampling', detail = "Individual record number", value = 0)
  
    # get number of records for each individual
    N$NRI  <- get_NRI(N, Time) 
    
    # update the dimension of the matrices with the maximum record number
    N$NR   <- max(N$NRI)
    
#     setProgress(detail = "Individual time sampling", value = 0.5)
    
    # get the sampling time for each individual, each trait and each population
    Tx     <- N$NS * (c(1:(N$NI*N$NT*N$NP)) - 1) + get_Tx_uniform(N, Time)
    Tx     <- as.numeric(na.omit(as.vector(t(Tx))))
    
#     setProgress(detail = "Sampling", value = 0.8)
    
    # get phenotype of the sampling individuals
    data_S <- data_C[Tx,]
    
#     setProgress(value = 1)
#     
#     return(data_S)
#   
#   })
  
  return(data_S)
}