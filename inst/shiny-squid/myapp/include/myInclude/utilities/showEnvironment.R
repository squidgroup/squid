showEnvironment <- function(input, myModule, myEnv){
  
  Env    <- setEnvironments(input, myModule)
  myVar  <- setVariables(input, myModule, Env)
  
  myEnv  <- get_environment(Env[[myEnv]], myVar$N, TRUE)
  
#   myPlot <- dygraph(myEnv, main = "New Haven Temperatures")
#   
#   return(myPlot)
  return(plot(y = myEnv, 
              x = rep(1:myVar$N$NS, myVar$N$NI), 
              col=rep(1:myVar$N$NI, each = myVar$N$NS), 
              pch= 16,
              xlab = "Time", 
              ylab = "Environment"))
}

