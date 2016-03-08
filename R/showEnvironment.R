#' showEnvironment
#'
#' @description          create and return simulated environment
#'
#' @param input          list of the model variables.
#' @param module         character of the module name
#' @param envName        character of the name of the environment ("X1", "X2").
#'
#' @return               vector of number of records per individual and trait
#' @export
#'
showEnvironment <- function(input, module, envName){
  
  sep       <- ifelse(is.null(module), "", "_")
  envObject <- setEnvironments(input, module, sep)
  modelVar  <- setVariables(input, module, envObject, sep)
  envData   <- getEnvironment(envObject[[envName]], modelVar$N, TRUE)
  
  myData   <- data.frame("envData"  = envData, 
                         "x"        = rep(1:modelVar$N$NS, modelVar$N$NI),
                         "colour"   = as.factor(rep(1:modelVar$N$NI, each = modelVar$N$NS)))
  
  myPlot   <- ggplot2::ggplot(myData, ggplot2::aes(y=envData, x=x, colour=colour)) +
                              ggplot2::geom_point() +
                              ggplot2::xlab("Time") +
                              ggplot2::ylab("Environment") + 
                              ggplot2::theme(legend.position="none")
  
  return(myPlot)
}

