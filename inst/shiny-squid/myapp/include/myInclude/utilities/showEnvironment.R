showEnvironment <- function(input, myModule, myEnv){
  
  Env      <- setEnvironments(input, myModule)
  myVar    <- setVariables(input, myModule, Env)
  
  myEnv    <- get_environment(Env[[myEnv]], myVar$N, TRUE)
  
  myData   <- data.frame("myEnv"  = myEnv, 
                         "x"      = rep(1:myVar$N$NS, myVar$N$NI),
                         "colour" = as.factor(rep(1:myVar$N$NI, each = myVar$N$NS)))
  
  myFigure <- ggplot(myData, aes(y=myEnv, x=x, colour=colour)) +
                      geom_point() +
                      xlab("Time") +
                      ylab("Environment") + 
                      theme(legend.position="none")
  
  return(myFigure)
}

