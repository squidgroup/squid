# setEnvironments: set up environment objects
#
# @description          
# 
# @param input          list of all the inputs used to run the model.
# @param module         character of the name of the module.
# 
# @return               list of the environments used to generete SQUID world.
#
setEnvironments <- function(input, module, sep){
  
  # Create a list object for each environment
  X1 <- createEnvironment(input, module, "X1", sep)
  X2 <- createEnvironment(input, module, "X2", sep)
  
  #Interaction
  X_Interaction <-  paste(module,"X_Interaction", sep = sep)
  
  envionments <- list(
    "X1" = X1,
    "X2" = X2,
    "Interaction" = ifelse(X_Interaction %in% names(input) && X1$state && X2$state,try(check_one_boolean(obj=input[[X_Interaction]], 
    																																																		 obj_name=paste0("input[[",X_Interaction,"]]"), 
    																																																		 longcalling, 
    																																																		 expression, 
    																																																		 but.not.seen.in.Error)),FALSE)
  )
  
  return(envionments)
}