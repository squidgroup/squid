SVRGetSummaryVariances <- function(input,B,Vind,Ve,VG,NT,step,nb.IS,trait,X1_state,X2_state,X_Interaction) {
  
  myNT  <- as.numeric(input[[NT]]) 
  VCov  <- round(SQUID::Cor2CovMatrix(as.matrix(input[[Vind]])),2)
  
  B2    <- as.matrix(input[[B]])
  
  if(!input[[X1_state]]){
    VCov[seq(from=X1, to=(nb.IS*myNT), by=nb.IS), ] <- 0 
    VCov[ ,seq(from=X1, to=(nb.IS*myNT), by=nb.IS)] <- 0 
    B2[1, seq(from=X1, to=(nb.IS*myNT), by=nb.IS)] <- 0
  }
  if(!input[[X2_state]]){
    VCov[seq(from=X2, to=(nb.IS*myNT), by=nb.IS), ] <- 0 
    VCov[ ,seq(from=X2, to=(nb.IS*myNT), by=nb.IS)] <- 0
    B2[1, seq(from=X2, to=(nb.IS*myNT), by=nb.IS)] <- 0 
  }
  if(!input[[X_Interaction]]){
    VCov[seq(from=X1X2, to=(nb.IS*myNT), by=nb.IS), ] <- 0 
    VCov[ ,seq(from=X1X2, to=(nb.IS*myNT), by=nb.IS)] <- 0 
    B2[1, seq(from=X1X2, to=(nb.IS*myNT), by=nb.IS)] <- 0 
  }
  
  Variances <- c(NA, # Fixed effects
                 B2[X1+step]^2, # Vmean1
                 B2[X2+step]^2, # Vmean2
                 B2[X1X2+step]^2, # Vmean2
                 NA, # Random effects
                 VCov[B0+step, B0+step], #Vdev0
                 VCov[X1+step, X1+step], # Vslope1
                 VCov[X2+step, X2+step], # Vslope2
                 VCov[X1X2+step, X1X2+step], # Vslope12
#                  2*VCov[X1+step, B0+step], # Cov Intercept and Vslope1
#                  2*VCov[X2+step, B0+step], # Cov Intercept and Vslope2
#                  2*VCov[X1X2+step, B0+step], # Cov Intercept and Vslope12
#                  2*VCov[X2+step, X1+step], # Cov Vslope1 and Vslope2
#                  2*VCov[X1X2+step, X1+step], # Cov Vslope1 and Vslope12
#                  2*VCov[X1X2+step, X2+step], # Cov Vslope1 and Vslope12
                 input[[VG]], # group variance
                 input[[Ve]] # measurement error variance
                 )

  Vp             <- sum(Variances, na.rm = TRUE)
  VarProportions <- paste("(",as.character(round((Variances/ifelse(Vp != 0, Vp,1)) * 100,1)),"%)",sep="")
  
  Value       <- c(Variances,Vp)
  Proportion  <- c(VarProportions,"(100%)")
  
  Value[is.na(Value)] <- " "
  Proportion <- as.character(Proportion)
  Proportion[Proportion == "(NA%)"] <- " "
  
  myTable <- data.frame(paste(Value,Proportion))
  names(myTable) <- paste("Trait.", trait, sep="")
  
  
  return(myTable)
  
}