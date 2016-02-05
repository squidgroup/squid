#' getEnvironment
#'
#' @description          generate the model environment
#'
#' @return               vector of the environment values
#'
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
        outputTMP <- environment$lin$Intercept + (environment$lin$Slope*(1:N$NS))
        output    <- rep(outputTMP, NB)
      }else{
        Intercept <-  rep(rnorm(NB, environment$lin$Intercept, sqrt(environment$lin$V)), each = N$NS)
        Slope     <-  rep(rnorm(NB, environment$lin$Slope, sqrt(environment$lin$V)), each = N$NS)
        myTime    <-  rep(1:N$NS, NB)
        output    <-  Intercept + (Slope * myTime)
      }
    }
      
    # Cyclic environment effect
    if(environment$cyc$state){
      if(environment$cyc$shared){
        
        A <- abs(environment$cyc$Amplitude)       # |A| = the amplitude
        B <- (2*pi) / abs(environment$cyc$Period) # 2pi/|B| = the period
        C <- -1 * environment$cyc$Hshift * B      # -C/B = the phase shift (horizontal shift)
        h <- environment$cyc$Vshift               # vertical shift
        
        outputTMP <- (A*sin(B*(1:N$NS) + C)) + h
        outputTMP <- rep(outputTMP, NB)
        
      }else{
        
        Amplitude <-  rep(rnorm(NB, environment$cyc$Amplitude, sqrt(environment$cyc$V)), each = N$NS)
        Period    <-  rep(rnorm(NB, environment$cyc$Period,    sqrt(environment$cyc$V)), each = N$NS)
        Hshift    <-  rep(rnorm(NB, environment$cyc$Hshift,    sqrt(environment$cyc$V)), each = N$NS)
        Vshift    <-  rep(rnorm(NB, environment$cyc$Vshift,    sqrt(environment$cyc$V)), each = N$NS)
        
        A <- abs(Amplitude)       # |A| = the amplitude
        B <- (2*pi) / abs(Period) # 2pi/|B| = the period
        C <- -1 * Hshift * B      # -C/B = the phase shift (horizontal shift)f
        h <- Vshift               # vertical shift
        
        myTime    <-  rep(1:N$NS, NB)
        outputTMP <- (A*sin(B*(myTime) + C)) + h
      }
      output <- output + outputTMP
    }
    
    # Random environment effect
    if(environment$ran$state){
      
      if(environment$ran$shared){
        
        outputTMP <- rnorm(N$NS, environment$ran$Mu, sqrt(environment$ran$V))
        if(environment$ran$autocorrelation) outputTMP <- decayRate(outputTMP, environment$ran$corr, N$NS)
        outputTMP <- rep(outputTMP, NB)
        
      }else{
        outputTMP <-  rnorm(N$NS*NB, environment$ran$Mu, sqrt(environment$ran$V))
        if(environment$ran$autocorrelation){
          for(i in 1:NB)
            outputTMP[((N$NS*(i-1))+1):(N$NS*i)] <- decayRate(outputTMP[((N$NS*(i-1))+1):(N$NS*i)], environment$ran$corr, N$NS)
        }
      }
      
      output <- output + outputTMP
    }
    
    # Standardize envirnement (Variance = 1) (dividing each individual environment by the environment standard deviation)
    # Centering environment to 0 (Subtract environment mean)
    EnvironmentID <- rep(seq(1:NB), each = N$NS)
    SD            <- rep(by(output, EnvironmentID, sd), each = N$NS)
    MEAN          <- rep(by(output, EnvironmentID, mean), each = N$NS)
    output        <- (output - MEAN) / SD 
    
  }
  
  return(output)

}