# createEnvironment: create an environment object
#
# Args:
#   input:        list of all the inputs used to run the model.
#   module:       character of the name of the module.
#   environment:  character of the environment name.
#   sep             character of the seperator between the module name and the variable name.
#
# Returns:
#   list of an environment object.

createEnvironment <- function(input, module, environment, sep){
  
  sep2 <- ifelse(is.null(environment), "", "_")
  
  # extract environment parameters from the general inputs
  inputNames <- list(
    "state"               = paste(module, paste(environment, "state", sep=sep2), sep = sep),
    # Stochastic effect
    "sto_state"           = paste(module, paste(environment, "sto_state", sep=sep2), sep = sep),
    "sto_shared"          = paste(module, paste(environment, "sto_shared", sep=sep2), sep = sep),
    "sto_Mu"              = paste(module, paste(environment, "sto_Mu", sep=sep2), sep = sep),
    "sto_V"               = paste(module, paste(environment, "sto_V", sep=sep2), sep = sep),
    "sto_autocor_state"   = paste(module, paste(environment, "sto_autocor_state", sep=sep2), sep = sep),
    "sto_corr"            = paste(module, paste(environment, "sto_corr", sep=sep2), sep = sep),
    # Linear effect
    "lin_state"           = paste(module, paste(environment, "lin_state", sep=sep2), sep = sep),
    "lin_shared"          = paste(module, paste(environment, "lin_shared", sep=sep2), sep = sep),
    "lin_V"               = paste(module, paste(environment, "lin_V", sep=sep2), sep = sep),
    "lin_intercept"       = paste(module, paste(environment, "lin_intercept", sep=sep2), sep = sep),
    "lin_slope"           = paste(module, paste(environment, "lin_slope", sep=sep2), sep = sep),
    # Cyclic effect
    "cyc_state"           = paste(module, paste(environment, "cyc_state", sep=sep2), sep = sep),
    "cyc_shared"          = paste(module, paste(environment, "cyc_shared", sep=sep2), sep = sep),
    "cyc_amplitude"       = paste(module, paste(environment, "cyc_amplitude", sep=sep2), sep = sep),
    "cyc_period"          = paste(module, paste(environment, "cyc_period", sep=sep2), sep = sep),
    "cyc_Hshift"          = paste(module, paste(environment, "cyc_Hshift", sep=sep2), sep = sep),
    "cyc_Vshift"          = paste(module, paste(environment, "cyc_Vshift", sep=sep2), sep = sep),
    "cyc_V"               = paste(module, paste(environment, "cyc_V", sep=sep2), sep = sep)
  )
  
  # Create a list of the environment object
  environmentObject <-  list(
    
    # the state of the environment (if TRUE is activated otherwise it's not)
    "state"  = ifelse(inputNames$state %in% names(input),
                      error_management(input[[inputNames$state]], 
                                       inputNames$state, 
                                       "check_one_boolean"),
                      FALSE),
    
    # Stochastic effect (normal distribution)
    #   state: the state of the stichastic environmental effect (TRUE is activated, FALSE is not)
    #   shared: if this effect is shared among individuals (TRUE is shared, FLASE is not)
    #   Mu: mean of the normal distribution
    #   v: variance of the normal distribution
    #   autocorrelation: utilization of autocorrelation (TRUE autorrelation is added FALSE is not)
    #   corr: correlation value between two consecutive values used for the autocorrelation (between 0 and 1)
    "sto"  = list("state"           = ifelse(inputNames$sto_state  %in% names(input), 
                                             error_management(input[[inputNames$sto_state]], 
                                                              inputNames$sto_state, 
                                                              "check_one_boolean"),
                                             FALSE),
                  "shared"          = ifelse(inputNames$sto_shared %in% names(input),
                                             error_management(input[[inputNames$sto_shared]], 
                                                              inputNames$sto_shared, 
                                                              "check_one_boolean"),
                                             TRUE),
                  "Mu"              = 0,
                  "V"               = ifelse(inputNames$sto_V %in% names(input),
                                             error_management(input[[inputNames$sto_V]], 
                                                              inputNames$sto_V, 
                                                              "check_one_numeric",
                                                              minimum=0),
                                             1),
                  "autocor_state"   = ifelse(inputNames$sto_autocor_state %in% names(input), 
                                             error_management(input[[inputNames$sto_autocor_state]], 
                                                              inputNames$sto_autocor_state, 
                                                              "check_one_boolean"),
                                             FALSE), 
                  "corr"            = ifelse(inputNames$sto_corr %in% names(input),
                                             error_management(input[[inputNames$sto_corr]], 
                                                              inputNames$sto_corr, 
                                                              "check_one_numeric", 
                                                              minimum=0,
                                                              maximum=1),
                                             0)),
    
    # Linear effect (environment = Intercept + Slope x Time)
    #   state: the state of the linear environmental effect (TRUE is activated, FALSE is not)
    #   shared: if this effect is shared among individuals (TRUE is shared, FLASE is not)
    #   v: variance of among-individual environments (noraml distribution arounf the mean of the intercept and the slope)
    #   Intercept: intercept of the linear equation
    #   Slope: slope of the linear equation
    "lin"  = list("state"     = ifelse(inputNames$lin_state %in% names(input), 
                                       error_management(input[[inputNames$lin_state]], 
                                                        inputNames$lin_state, 
                                                        "check_one_boolean"),
                                       FALSE),
                  "shared"    = ifelse(inputNames$lin_shared %in% names(input),
                                       error_management(input[[inputNames$lin_shared]], 
                                                        inputNames$lin_shared, 
                                                        "check_one_boolean"),
                                       TRUE),
                  "intercept" = ifelse(inputNames$lin_intercept %in% names(input),
                                       error_management(input[[inputNames$lin_intercept]], 
                                                        inputNames$lin_intercept, 
                                                        "check_one_numeric"),
                                       0),
                  "slope"     = ifelse(inputNames$lin_slope %in% names(input),
                                       error_management(input[[inputNames$lin_slope]], 
                                                        inputNames$lin_slope, 
                                                        "check_one_numeric"),
                                       1),
                  "V"         = ifelse(inputNames$lin_V %in% names(input), 
                                       error_management(input[[inputNames$lin_V]], 
                                                        inputNames$lin_V, 
                                                        "check_one_numeric",
                                                        minimum=0),
                                       1)),
    
    # Linear effect (environment = Intercept + Slope x Time)
    #   state: the state of the linear environmental effect (TRUE is activated, FALSE is not)
    #   shared: if this effect is shared among individuals (TRUE is shared, FLASE is not)
    #   v: variance of among-individual environments (noraml distribution arounf the mean of the intercept and the slope)
    #   Intercept: intercept of the linear equation
    #   Slope: slope of the linear equation
    "cyc"  = list("state"     = ifelse(inputNames$cyc_state %in% names(input),
                                       error_management(input[[inputNames$cyc_state]], 
                                                        inputNames$cyc_state, 
                                                        "check_one_boolean"),
                                       FALSE),
                  "shared"    = ifelse(inputNames$cyc_shared %in% names(input),
                                       error_management(input[[inputNames$cyc_shared]], 
                                                        inputNames$cyc_shared, 
                                                        "check_one_boolean"),
                                       TRUE),
                  "amplitude" = ifelse(inputNames$cyc_amplitude %in% names(input),
                                       error_management(input[[inputNames$cyc_amplitude]], 
                                                        inputNames$cyc_amplitude, 
                                                        "check_one_numeric",
                                                        minimum=0),
                                       10),
                  "period"    = ifelse(inputNames$cyc_period %in% names(input),
                                       error_management(input[[inputNames$cyc_period]], 
                                                        inputNames$cyc_period, 
                                                        "check_one_numeric",
                                                        minimum=0),
                                       10),
                  "Hshift"    = ifelse(inputNames$cyc_Hshift %in% names(input),
                                       error_management(input[[inputNames$cyc_Hshift]], 
                                                        inputNames$cyc_Hshift, 
                                                        "check_one_numeric"),
                                       0), 
                  "Vshift"    = ifelse(inputNames$cyc_Vshift %in% names(input),
                                       error_management(input[[inputNames$cyc_Vshift]], 
                                                        inputNames$cyc_Vshift, 
                                                        "check_one_numeric"),
                                       0),
                  "V"         = ifelse(inputNames$cyc_V %in% names(input),
                                       error_management(input[[inputNames$cyc_V]], 
                                                        inputNames$cyc_V, 
                                                        "check_one_numeric",
                                                        minimum=0),
                                       1))
  )
  
  return(environmentObject)
}


# setEnvironments: set up environment objects
#
# Args:
#   input           list of all the inputs used to run the model.
#   module          character of the name of the module.
#   sep             character of the seperator between the module name and the variable name.
#
# Returns:
#   list of the environments used to generete SQUID world.

setEnvironments <- function(input, module, sep){
  
  # Create a list object for each environment
  X1 <- createEnvironment(input, module, "X1", sep)
  X2 <- createEnvironment(input, module, "X2", sep)
  
  #Interaction
  X_Interaction <-  paste(module,"X_Interaction", sep = sep)
  
  envionments <- list(
    "X1" = X1,
    "X2" = X2,
    "Interaction" = ifelse(X_Interaction %in% names(input) && X1$state && X2$state,
    											 error_management(input[[X_Interaction]], 
																						X_Interaction, 
																						"check_one_boolean"),
    											 FALSE)
  )
  
  return(envionments)
}


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