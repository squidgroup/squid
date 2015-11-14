#Server functions for the full model
SVRGetBMatrix <- function(ID,NT, X1_state, X2_state, 
                          X_Interaction, 
                          BisNew, myB) {
  
  NT <- as.numeric(NT)
  
  inputOn <- matrix(rep(c(TRUE,
                          ifelse(X1_state,TRUE,FALSE),
                          ifelse(X2_state,TRUE,FALSE),
                          ifelse(X_Interaction,TRUE,FALSE)),
                          NT),1)
  
  isolate({
    if(!BisNew){ 
      B            <- matrix(rep(0,NT*nb.IS),1)      
      newSize      <- ifelse(NT*nb.IS > length(myB),length(myB),NT*nb.IS)      
      B[1:newSize] <- myB[1:newSize]   
      
    }else{ 
      B            <- matrix(rep(0,NT*nb.IS),1)            
      B[1:nb.IS]   <- FullModel_VAR$B$value[1:nb.IS] 
    }
    
    B[which(is.na(B))] <- 0
    
  })
  
  B              <- data.frame(B)
  colnames(B)    <- FullModel_VAR$Bnames[1:(NT*nb.IS)] 

  return(matrixInputB(ID, "", B, inputOn, NT, FullModel_VAR$NTnames, nb.IS))
  
}