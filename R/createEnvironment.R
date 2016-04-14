# createEnvironment
#
# @description          create an environment object
# 
# @param input          list of all the inputs used to run the model.
# @param module         character of the name of the module.
# @param environment    character of the environment name.
#
# @return               list of an environment object.
#
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