squidR <- function(input=list(), plot=FALSE, data=NULL, module=NULL, X_previsualization=NULL){ 
  
  # Main function of the full model that simulates individual phenotypes over time
  # and then samples within those phenotypes according to specific sampling design
  #
  # Args:
  #   input: list contains all the inputs used to run the model 
  #          (for more details see setEnvironments and setVariables functions).
  #   module: name of the module.
  
  # Returns:
  #   Return a list with:
  #     full_Data   : continous phenotypic data (raw data)
  #     sampled_Data: sampled phenotypic data
  #     myPlot      : plots of the simulation results
  
  ##############################################################
  #################### INPUT VARIABLES  ########################
  ##############################################################  
  
  # Set seperator character
  sep <- ifelse(is.null(module), "", "_")
  
  # Set up environmental variables
  environments <- setEnvironments(input, module, sep)
  
  # Set up other variables
  variables    <- setVariables(input, module, environments, sep)

  Mu        <- variables$Mu
  N         <- variables$N
  B         <- variables$B
  V         <- variables$V
  Time      <- variables$Time
  variables <- variables$Variables
  
  if(is.null(X_previsualization)){
    
    # Initialize output
    output <- list()
    
    #######################################################################################
    ## Generate my phenotype traits
    if(is.null(data)){
      output[["full_data"]]    <- getFullData(Mu, N, B, r, V, Time, variables, environments)
    }else{
      output[["full_data"]]    <- data
    }
    
    ####################################################################################### 
    ## Get Sampling data  
    output[["sampled_data"]] <- getSampledData(N, Time, output[["full_data"]])
    
    #######################################################################################
    ## Display results
    if(plot) 
      output[["plots"]]     <- displayResults(N, Time, output[["full_data"]], output[["sampled_data"]])
    
    #######################################################################################
    
  }else{
    
    if(X_previsualization %in% c("X1", "X2")){
      ### Generate environment for previsualization
      X <- getEnvironment(environments[[X_previsualization]], N, TRUE)
      
      myData   <- data.frame("envData"  = X, 
                             "x"        = rep(1:N$NS, N$NI),
                             "colour"   = as.factor(rep(1:N$NI, each = N$NS)))
      
      output   <- ggplot2::ggplot(myData, ggplot2::aes(y=envData, x=x, colour=colour)) +
                                  ggplot2::geom_point() +
                                  ggplot2::xlab("Time") +
                                  ggplot2::ylab("Environment") + 
                                  ggplot2::theme(legend.position="none")
    }else{
      stop("X_previsualization is not a valid environment tag.")
    }
  }
  
  return(output)
  
}