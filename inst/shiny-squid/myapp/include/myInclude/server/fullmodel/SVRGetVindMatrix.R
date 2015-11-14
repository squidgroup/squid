#Server functions for the full model
SVRGetVindMatrix <- function(ID,NT, X1_state, X2_state, 
                             X_Interaction, 
                             VindisNew, myVind) {
  
  NT <- as.numeric(NT)    
  
  inputOn <- matrix(rep(TRUE,(nb.IS*NT)^2),nb.IS*NT)
  
  if(!X1_state){
    inputOn[seq(from=X1,to=(NT*nb.IS),by=nb.IS),] <- FALSE
    inputOn[,seq(from=X1,to=(NT*nb.IS),by=nb.IS)] <- FALSE
  }
  if(!X2_state){ 
    inputOn[seq(from=X2,to=(NT*nb.IS),by=nb.IS),]  <- FALSE
    inputOn[,seq(from=X2,to=(NT*nb.IS),by=nb.IS)] <- FALSE
  }
  if(!X_Interaction){ 
    inputOn[seq(from=X1X2,to=(NT*nb.IS),by=nb.IS),] <- FALSE
    inputOn[,seq(from=X1X2,to=(NT*nb.IS),by=nb.IS)] <- FALSE
  }
  
  isolate({
    if(!VindisNew){ 
      Vind                           <- matrix(rep(0,(nb.IS*NT)^2),nb.IS*NT)      
      newSize                        <- ifelse(NT*nb.IS > dim(myVind)[1],dim(myVind)[1],NT*nb.IS)      
      Vind[1:newSize, 1:newSize]     <- myVind[1:newSize, 1:newSize]
    }else{ 
      Vind         <- matrix(rep(0,(nb.IS*NT)^2),nb.IS*NT) 
      Vind[1:nb.IS, 1:nb.IS]     <- FullModel_VAR$Vind$value[1:nb.IS, 1:nb.IS] 
    }
    
    Vind[which(is.na(Vind))] <- 0
  })
  
  Vind           <- data.frame(Vind)
  colnames(Vind) <- FullModel_VAR$Vindnames[1:(NT*nb.IS)]

  return(matrixInputVind(ID, "", Vind, inputOn, NT, FullModel_VAR$NTnames, nb.IS))
  
}