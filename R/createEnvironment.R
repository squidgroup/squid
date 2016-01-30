#' createEnvironment
#'
#' @description          create an environment object
#' 
#' @param input          list of all the inputs used to run the model.
#' @param module         character of the name of the module.
#' @param environment    character of the environment name.
#'
#' @return               list of an environment object.
#'
createEnvironment <- function(input, module, environment){
  
  # extract environment parameters from the general inputs
  inputNames <- list(
    "state"               = paste(module, environment, "state", sep = "_"),
    # "myModule"            = paste(myModule, myEnv, "myModule", sep = "_"),
    # Random effect
    "ran_state"           = paste(module, environment, "ran_state", sep = "_"),
    "ran_shared"          = paste(module, environment, "ran_shared", sep = "_"),
    "ran_Mu"              = paste(module, environment, "ran_Mu", sep = "_"),
    "ran_V"               = paste(module, environment, "ran_V", sep = "_"),
    "ran_autocorrelation" = paste(module, environment, "ran_autocorrelation", sep = "_"),
    "ran_corr"            = paste(module, environment, "ran_corr", sep = "_"),
    # Linear effect
    "lin_state"           = paste(module, environment, "lin_state", sep = "_"),
    "lin_shared"          = paste(module, environment, "lin_shared", sep = "_"),
    "lin_V"               = paste(module, environment, "lin_V", sep = "_"),
    "lin_Intercept"       = paste(module, environment, "lin_Intercept", sep = "_"),
    "lin_Slope"           = paste(module, environment, "lin_Slope", sep = "_"),
    # Cyclic effect
    "cyc_state"           = paste(module, environment, "cyc_state", sep = "_"),
    "cyc_shared"          = paste(module, environment, "cyc_shared", sep = "_"),
    "cyc_V"               = paste(module, environment, "cyc_V", sep = "_"),
    "cyc_Amplitude"       = paste(module, environment, "cyc_Amplitude", sep = "_"),
    "cyc_Period"          = paste(module, environment, "cyc_Period", sep = "_"),
    "cyc_Hshift"          = paste(module, environment, "cyc_Hshift", sep = "_"),
    "cyc_Vshift"          = paste(module, environment, "cyc_Vshift", sep = "_")
  )
  
  # Create a list of the environment object
  environmentObject <-  list(
    
    # the state of the environment (if TRUE is activated otherwise it's not)
    "state"  = ifelse(inputNames$state %in% names(input),input[[inputNames$state]],FALSE),
    
    # Random effect (normal distribution)
    #   state: the state of the random environmental effect (TRUE is activated, FALSE is not)
    #   shared: if this effect is shared among individuals (TRUE is shared, FLASE is not)
    #   Mu: mean of the normal distribution
    #   v: variance of the normal distribution
    #   autocorrelation: utilization of autocorrelation (TRUE autorrelation is added FALSE is not)
    #   corr: correlation value between two consecutive values used for the autocorrelation (between 0 and 1)
    "ran"  = list("state"           = ifelse(inputNames$ran_state %in% names(input),input[[inputNames$ran_state]],FALSE),
                  "shared"          = ifelse(inputNames$ran_shared %in% names(input),input[[inputNames$ran_shared]],TRUE),
                  "Mu"              = 0,
                  "V"               = ifelse(inputNames$ran_V %in% names(input),input[[inputNames$ran_V]],1),
                  "autocorrelation" = ifelse(inputNames$ran_autocorrelation %in% names(input),input[[inputNames$ran_autocorrelation]],FALSE), 
                  "corr"            = ifelse(inputNames$ran_corr %in% names(input),input[[inputNames$ran_corr]],0)),
    
    # Linear effect (environment = Intercept + Slope x Time)
    #   state: the state of the linear environmental effect (TRUE is activated, FALSE is not)
    #   shared: if this effect is shared among individuals (TRUE is shared, FLASE is not)
    #   v: variance of among-individual environments (noraml distribution arounf the mean of the intercept and the slope)
    #   Intercept: intercept of the linear equation
    #   Slope: slope of the linear equation
    "lin"  = list("state"     = ifelse(inputNames$lin_state %in% names(input),input[[inputNames$lin_state]],FALSE),
                  "shared"    = ifelse(inputNames$lin_shared %in% names(input),input[[inputNames$lin_shared]],TRUE),
                  "Intercept" = ifelse(inputNames$lin_Intercept %in% names(input),input[[inputNames$lin_Intercept]],0),
                  "Slope"     = ifelse(inputNames$lin_Slope %in% names(input),input[[inputNames$lin_Slope]],1),
                  "V"         = ifelse(inputNames$lin_V %in% names(input),input[[inputNames$lin_V]],1)),
    
    # Linear effect (environment = Intercept + Slope x Time)
    #   state: the state of the linear environmental effect (TRUE is activated, FALSE is not)
    #   shared: if this effect is shared among individuals (TRUE is shared, FLASE is not)
    #   v: variance of among-individual environments (noraml distribution arounf the mean of the intercept and the slope)
    #   Intercept: intercept of the linear equation
    #   Slope: slope of the linear equation
    "cyc"  = list("state"     = ifelse(inputNames$cyc_state %in% names(input),input[[inputNames$cyc_state]],FALSE),
                  "shared"    = ifelse(inputNames$cyc_shared %in% names(input),input[[inputNames$cyc_shared]],TRUE),
                  "Amplitude" = ifelse(inputNames$cyc_Amplitude %in% names(input),input[[inputNames$cyc_Amplitude]],10),
                  "Period"    = ifelse(inputNames$cyc_Period %in% names(input),input[[inputNames$cyc_Period]],10),
                  "Hshift"    = ifelse(inputNames$cyc_Hshift %in% names(input),input[[inputNames$cyc_Hshift]],0), 
                  "Vshift"    = ifelse(inputNames$cyc_Vshift %in% names(input),input[[inputNames$cyc_Vshift]],0),
                  "V"         = ifelse(inputNames$cyc_V %in% names(input),input[[inputNames$cyc_V]],1))
  )
  
  return(environmentObject)
}