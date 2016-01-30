
runPowerAnalysis <- function(input, ModStep, NI, NR){

  input2 <- reactiveValuesToList(input)
  res    <- NULL
  
  for(index in 1:length(NI)){
    
      input2[[paste0(ModStep,"_NI")]] <- NI[index] 
      input2[[paste0(ModStep,"_NR")]] <- NR[index] 
    
      df <- SQUID::runSQUIDfct(input2, ModStep) 
      
      df <- df$sampled_Data
      df <- df %>% 
              select(Replicate, Individual, Phenotype, X1) %>% 
              mutate(nIndividual=paste0("NI=",NI[index]), nRecord=paste0("NR=",NR[index]))
      
      res <- rbind(res, df)
  }
  
  lmerRes <- res %>%
                group_by(Replicate, nIndividual, nRecord) %>%
                do(lmerall(.)) %>%
                tbl_df()

  return(lmerRes)
}

lmerall <- function(df){    
  modIDS      <- lmer(Phenotype~ X1 + (X1|Individual), data=df)
  vcs         <- VarCorr(modIDS)
  Vintercepts <- vcs$Individual[1,1]
  Vslopes     <- vcs$Individual[2,2]
  corIS       <- cov2cor(vcs$Individual[,])[2]
  res         <- data.frame("Parameter"  = c("Vi", "Vs", "CORis"), 
                            "Value"      = c(Vintercepts, Vslopes, corIS))
  return(res)
}
