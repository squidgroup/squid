
# Nb        : number of time step
# Mu_rand   : mean of the random normal distribution
# V_rand    : variance of the random normal distribution
# Intercept : intercept of the linear distribution
# Slope     : slope of the linear distribution
# V_auto    : variance of the autocorrelate distribution
get_environment <- function(Env, N, Vizualisation){
  
  if(!Vizualisation){
    output <- vector("double", N$NS*N$NI*N$NP)
    NB     <- N$NI*N$NP
  }else{
    output <- vector("double", N$NS*N$NI)
    NB     <- N$NI
  }
  
  if(Env$state){
    
    # Linear environment effect
    if(Env$lin$state){
      if(Env$lin$shared){ 
        outputTMP <- Env$lin$Intercept + (Env$lin$Slope*(1:N$NS))
        output    <- rep(outputTMP, NB)
      }else{
        Intercept <-  rep(rnorm(NB, Env$lin$Intercept, sqrt(Env$lin$V)), each = N$NS)
        Slope     <-  rep(rnorm(NB, Env$lin$Slope, sqrt(Env$lin$V)), each = N$NS)
        myTime    <-  rep(1:N$NS, NB)
        output    <-  Intercept + (Slope * myTime)
      }
    }
      
    # Cyclic environment effect
    if(Env$cyc$state){
      if(Env$cyc$shared){
        
        A <- abs(Env$cyc$Amplitude)       # |A| = the amplitude
        B <- (2*pi) / abs(Env$cyc$Period) # 2pi/|B| = the period
        C <- -1 * Env$cyc$Hshift * B      # -C/B = the phase shift (horizontal shift)
        h <- Env$cyc$Vshift               # vertical shift
        
        outputTMP <- (A*sin(B*(1:N$NS) + C)) + h
        outputTMP <- rep(outputTMP, NB)
        
      }else{
        
        Amplitude <-  rep(rnorm(NB, Env$cyc$Amplitude, sqrt(Env$cyc$V)), each = N$NS)
        Period    <-  rep(rnorm(NB, Env$cyc$Period,    sqrt(Env$cyc$V)), each = N$NS)
        Hshift    <-  rep(rnorm(NB, Env$cyc$Hshift,    sqrt(Env$cyc$V)), each = N$NS)
        Vshift    <-  rep(rnorm(NB, Env$cyc$Vshift,    sqrt(Env$cyc$V)), each = N$NS)
        
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
    if(Env$ran$state){
      
      if(Env$ran$shared){
        
        outputTMP <- rnorm(N$NS, Env$ran$Mu, sqrt(Env$ran$V))
        if(Env$ran$autocorrelation) outputTMP <- decay_rate(outputTMP, Env$ran$corr, N$NS)
        outputTMP <- rep(outputTMP, NB)
        
      }else{
        outputTMP <-  rnorm(N$NS*NB, Env$ran$Mu, sqrt(Env$ran$V))
        if(Env$ran$autocorrelation){
          for(i in 1:NB)
            outputTMP[((N$NS*(i-1))+1):(N$NS*i)] <- decay_rate(outputTMP[((N$NS*(i-1))+1):(N$NS*i)], Env$ran$corr, N$NS)
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