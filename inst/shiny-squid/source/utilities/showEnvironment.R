showEnvironment <- function(input, module, envName){
  
  envObject <- squid::setEnvironments(input, module)
  modelVar  <- squid::setVariables(input, module, envObject)
  envData   <- squid::getEnvironment(envObject[[envName]], modelVar$N, TRUE)
  
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

