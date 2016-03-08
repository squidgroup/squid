
runPowerAnalysis <- function(input, ModStep, NI, NR){

  input2 <- reactiveValuesToList(input)
  res    <- NULL
  
  for(index in 1:length(NI)){
    
      input2[[paste0(ModStep,"_NI")]] <- NI[index] 
      input2[[paste0(ModStep,"_NR")]] <- NR[index] 
    
      df <- SQUID::runSQUIDfct(input2, ModStep)
      
#       df <- as.data.table(df$sampled_Data)
#       df <- df[ , .(Replicate, Individual, Phenotype, X1)]
#       df[ , ':=' (nIndividual = paste0("NI=",NI[index]), 
#                   nRecord     = paste0("NR=",NR[index]))]

      df <- df$sampled_Data
      df <- df %>% 
              dplyr::select(Replicate, Individual, Phenotype, X1) %>% 
              dplyr::mutate(nIndividual=paste0("NI=",NI[index]), nRecord=paste0("NR=",NR[index]))
      
      res <- rbind(res, df)
      
  }

  # res[ ,, by=list(Replicate, nIndividual, nRecord)]
  
  lmerRes <- res %>%
                dplyr::group_by(Replicate, nIndividual, nRecord) %>%
                dplyr::do(lmerall(.)) %>%
                dplyr::tbl_df()

  return(lmerRes)
}

lmerall <- function(df){    
  modIDS      <- lme4::lmer(Phenotype~ X1 + (X1|Individual), data=df)
  vcs         <- lme4::VarCorr(modIDS)
  Vintercepts <- vcs$Individual[1,1]
  Vslopes     <- vcs$Individual[2,2]
  corIS       <- stats::cov2cor(vcs$Individual[,])[2]
  res         <- data.frame("Parameter"  = c("Vi", "Vs", "CORis"), 
                            "Value"      = c(Vintercepts, Vslopes, corIS))
  return(res)
}
