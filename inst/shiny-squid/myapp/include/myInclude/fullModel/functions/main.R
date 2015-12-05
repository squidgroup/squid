main <- function(input, myModule, session=NULL, progress=FALSE){ 
  
# Main function of the full model that simulates individual phenotypes over time
# and then samples within those phenotypes according to specific sampling design
#
# Args:
#   input: list contains all the inputs used to run the model 
#          (for more details see setEnvironments and setVariables functions).
#   myModule: name of the module.
#   session: shiny session object.
#   progress: progress bar object.
  
# Returns:
#   Return a list that contains:
#     myPlot: plots of the simulation results
#     data_C: continous phenotypic data (raw data)
#     data_S: sampled phenotypic data
#     V: list of variances associated with the model  
  
  
  
  # Start the clock!
#   ptm <- proc.time()
  
  ##############################################################
  #################### INPUT VARIABLES  ########################
  ##############################################################  
  
  # Set up environmental variables
  Env    <- setEnvironments(input, myModule)
  
  # Set up other variables
  myVar  <- setVariables(input, myModule, Env)
  
  Mu        <- myVar$Mu
  N         <- myVar$N
  B         <- myVar$B
  V         <- myVar$V
  Time      <- myVar$Time
  Variables <- myVar$Variables
  
#   cat("bar\n", file=stderr())

  #######################################################################################
  ## Generate my phenotype traits
  data_C     <- get_data_C(Mu, N, B, r, V, Time, Variables, Env, session, progress)

  ####################################################################################### 
  ## Get Sampling data  
  data_S     <- get_data_S(N, Time, data_C, session, progress)

  #######################################################################################
  ## Display results  
  myPlot     <- display_results(N, Time, data_C, data_S)
  
  #######################################################################################
  # Stop clock timer
#   Time_clock <- proc.time() - ptm
#   cat(Time_clock)
  
  return(list("myPlot"=myPlot, "data_C"=data_C, "data_S"=data_S, "V"=V))

}
