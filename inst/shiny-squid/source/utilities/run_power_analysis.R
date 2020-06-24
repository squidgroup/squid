runPowerAnalysis <- function(input, ModStep, NI, NR){

  input2 <- reactiveValuesToList(input)
  res    <- NULL
  
  for(index in 1:length(NI)){
    
      input2[[paste0(ModStep,"_NI")]] <- NI[index] 
      input2[[paste0(ModStep,"_NR")]] <- NR[index] 
    
      df <- squid::squidR(input2, module=ModStep)
      
      df <- as.data.table(df$sampled_data)
      df <- df[ , .(Replicate, Individual, Phenotype, X1)]
      df[ , ':=' (nIndividual = paste0("NI=",NI[index]), 
                  nRecord     = paste0("NR=",NR[index]))]
      
      res <- rbind(res, df)
  }
  
  keycols = c("Replicate", "nIndividual", "nRecord")
  setkeyv(res, keycols)
  lmerRes  <- res[ ,.(Parameter=c("Vi", "Vs", "CORis"), Value=lmerall(.SD)), 
  								by=list(Replicate, nIndividual, nRecord),
  								.SDcols=c("Phenotype", "X1", "Individual")]

  return(lmerRes)
}

lmerall <- function(df){
  modIDS      <- lme4::lmer(Phenotype~ X1 + (X1|Individual), data=df)
  vcs         <- lme4::VarCorr(modIDS)
  Vintercepts <- vcs$Individual[1,1]
  Vslopes     <- vcs$Individual[2,2]
  corIS       <- stats::cov2cor(vcs$Individual[,])[2]
  res         <- c(Vintercepts, Vslopes, corIS)
  return(res)
}
