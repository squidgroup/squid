
# Nb        : number of time step
# Mu_rand   : mean of the random normal distribution
# V_rand    : variance of the random normal distribution
# Intercept : intercept of the linear distribution
# Slope     : slope of the linear distribution
# V_auto    : variance of the autocorrelate distribution
get_environment_old <- function(Env, N){
  
  output <- NULL
  
  switch(Env$type, 
         
         # random with a normal distribution
         ran ={ 
           if(Env$shared){             
             output <- rnorm(N$NS, Env$ran$Mu, sqrt(Env$ran$V))           
             if(Env$ran$decay)  output <- decay_rate(output, Env$ran$alpha, N$NS)
#                output <- cppDecayRate(output, Env$ran$alpha, N$NS)
#                output <- decay_rate(output, Env$ran$alpha, N$NS)
              output <- rep(output, N$NI*N$NP)             
           }else{
             for(i in 1:(N$NI*N$NP)){
               env_temp <- rnorm(N$NS, Env$ran$Mu, sqrt(Env$ran$V))
               if(Env$ran$decay) env_temp <- decay_rate(env_temp, Env$ran$alpha, N$NS) 
#                  env_temp <- cppDecayRate(env_temp, Env$ran$alpha, N$NS)
#                  env_temp <- decay_rate(env_temp, Env$ran$alpha, N$NS)                                          
               output <- c(output,env_temp)             
             }             
           }                        
         }, 
         
         # Linear
         lin ={ if(Env$shared){
           output <- Env$lin$Intercept + (Env$lin$Slope*(1:N$NS))
           output <- rep(output, N$NI*N$NP)
         }else{
           for(i in 1:(N$NI*N$NP)){
             env_temp <- Env$lin$Intercept + (Env$lin$Slope*(1:N$NS))               
             output   <- c(output,env_temp)
           }                     
         }  
         }, 
         
         # Cyclic (Sinusoidal)
         cyc ={  
           A <- Env$cyc$Amplitude # |A| = the amplitude
           B <- Env$cyc$Period    # 2pi/|B| = the period
           C <- Env$cyc$Hshift    # -C/B = the phase shift (horizontal shift)f
           h <- Env$cyc$Vshift    # vertical shift
           
           if(Env$shared){                                  
             output <- (A*sin(B*(1:N$NS) + C)) + h
             output <- rep(output, N$NI*N$NP)              
           }else{
             for(i in 1:(N$NI*N$NP)){
               env_temp <- (A*sin(B*(1:N$NS) + C)) + h               
               output   <- c(output,env_temp)
             }
           }              
         },                   
         
        {print('Default')} # Default
  )
  
  
  return(output)

}