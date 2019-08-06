# squidR: Run SQuID without graphical user interface
#
# Args:
#   input	              A list of the model input parameters (see Details).
#   plot	              logical; If FALSE (default), squidR does not include the simulation plots into the output list (see Value).
#   data	              A data.frame of the full data (the "world") returned by squidR (output$full_data).
#   module              A character string of the module name. This argument is only used by SQuID app.
#   X_previsualization	A character string of the environment name. This argument is only used by SQuID app.
#
# Returns:
#   returns a list that includes a data.frame of the full data generated (the "world"), 
#   a data.frame of the sampled data and six ggplot2 plots of the results (only if plot is set to TRUE).
#
#' @export
#' @import data.table
#' 
squidR <- function(input=list(), plot=FALSE, data=NULL, module=NULL, X_previsualization=NULL){ 
  
  ##############################################################
  #################### INPUT VARIABLES  ########################
  ##############################################################  

  # check inputs
  if(!is.list(input)) 
    stop("input must be a list.")
  if(!is.logical(plot) || length(plot) != 1) 
    stop("plot must be one logical element.")
  if(!is.null(data) && (!is.data.frame(data) || ncol(data) != 20)) 
    stop("data must be the full data returned by squidR (output$full_data).")
  if(!is.null(module) && (!is.character(module) || length(module) != 1)) 
    stop("module must be a one string element.")
  
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
      output[["full_data"]]    <- getFullData(Mu, N, B, V, Time, variables, environments)
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
    
    if(length(X_previsualization) == 1 && X_previsualization %in% c("X1", "X2")){
      
      x<-r<-envData<-colour<-NULL
      
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