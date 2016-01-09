# setEnvironments: set up environment objects 
#
# Args:
#   input: list contains all the inputs used to run the model
#   myModule: name of the module.
#
# Returns:
#   Env: a list that contains all environment objects

setEnvironments <- function(input, myModule){
  
  # Create a list object for each environment
  X1 <- createEnvironment(input, myModule, "X1")
  X2 <- createEnvironment(input, myModule, "X2")
  
  #Interaction
  X_Interaction <-  paste(myModule,"X_Interaction", sep = "_")
  
  Env <- list(

    "X1" = X1,
    "X2" = X2,
    "Interaction" = ifelse(X_Interaction %in% names(input) && X1$state && X2$state,input[[X_Interaction]],FALSE)
  )
  
  return(Env)
}