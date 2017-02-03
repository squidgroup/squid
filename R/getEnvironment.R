# getEnvironment: generates environmental values
#
# Args:
#   environment:    List of the environment information.
#   N:              internal list of simulation variables (related to simulation design). 
#   visualization:  logical; TRUE if environment is needed only for visualization.
#
# Returns:
#   vector of the environment values

getEnvironment <- function(environment, N, visualization){
  
  if(!visualization){
    output <- vector("double", N$NS*N$NI*N$NP)
    NB     <- N$NI*N$NP
  }else{
    output <- vector("double", N$NS*N$NI)
    NB     <- N$NI
  }
  
  if(environment$state){
    
    # Linear environment effect
    if(environment$lin$state){
      if(environment$lin$shared){ 
        outputTMP <- environment$lin$intercept + (environment$lin$slope*(1:N$NS))
        output    <- rep(outputTMP, NB)
      }else{
        intercept <-  rep(stats::rnorm(NB, environment$lin$intercept, sqrt(environment$lin$V)), each = N$NS)
        slope     <-  rep(stats::rnorm(NB, environment$lin$slope, sqrt(environment$lin$V)), each = N$NS)
        myTime    <-  rep(1:N$NS, NB)
        output    <-  intercept + (slope * myTime)
      }
    }
      
    # Cyclic environment effect
    if(environment$cyc$state){
      if(environment$cyc$shared){
        
        A <- abs(environment$cyc$amplitude)       # |A| = the amplitude
        B <- (2*pi) / abs(environment$cyc$period) # 2pi/|B| = the period
        C <- -1 * environment$cyc$Hshift * B      # -C/B = the phase shift (horizontal shift)
        h <- environment$cyc$Vshift               # vertical shift
        
        outputTMP <- (A*sin(B*(1:N$NS) + C)) + h
        outputTMP <- rep(outputTMP, NB)
        
      }else{
        
        amplitude <-  rep(stats::rnorm(NB, environment$cyc$amplitude, sqrt(environment$cyc$V)), each = N$NS)
        period    <-  rep(stats::rnorm(NB, environment$cyc$period,    sqrt(environment$cyc$V)), each = N$NS)
        Hshift    <-  rep(stats::rnorm(NB, environment$cyc$Hshift,    sqrt(environment$cyc$V)), each = N$NS)
        Vshift    <-  rep(stats::rnorm(NB, environment$cyc$Vshift,    sqrt(environment$cyc$V)), each = N$NS)
        
        A <- abs(amplitude)       # |A| = the amplitude
        B <- (2*pi) / abs(period) # 2pi/|B| = the period
        C <- -1 * Hshift * B      # -C/B = the phase shift (horizontal shift)f
        h <- Vshift               # vertical shift
        
        myTime    <-  rep(1:N$NS, NB)
        outputTMP <- (A*sin(B*(myTime) + C)) + h
      }
      output <- output + outputTMP
    }
    
    # Stochastic environment effect
    if(environment$sto$state){
      
      if(environment$sto$shared){
        
        outputTMP <- stats::rnorm(N$NS, environment$sto$Mu, sqrt(environment$sto$V))
        
        if(environment$sto$autocor_state) outputTMP <- decayRate(outputTMP, environment$sto$corr, N$NS)
        outputTMP <- rep(outputTMP, NB)
        
      }else{
        outputTMP <-  stats::rnorm(N$NS*NB, environment$sto$Mu, sqrt(environment$sto$V))
        if(environment$sto$autocor_state){
          for(i in 1:NB)
            outputTMP[((N$NS*(i-1))+1):(N$NS*i)] <- decayRate(outputTMP[((N$NS*(i-1))+1):(N$NS*i)], environment$sto$corr, N$NS)
        }
      }
      
      output <- output + outputTMP
    }
    
    # Standardize envirnement (Variance = 1) (dividing each individual environment by the environment standard deviation)
    # Centering environment to 0 (Subtract environment mean)
  	if(!visualization){ # Do not standardize environment when it's just for previsualization
	  	EnvironmentID <- rep(seq(1:NB), each = N$NS)
	    SD            <- rep(by(output, EnvironmentID, stats::sd), each = N$NS)
	    MEAN          <- rep(by(output, EnvironmentID, base::mean), each = N$NS)
	    output        <- (output - MEAN) / SD
  	}
    
  }
  
  return(output)
}