
lmerall <- function(df){    
  modIDS      <- lme4::lmer(Phenotype~ X1 + (X1|Individual), data=df)
  vcs         <- VarCorr(modIDS)
  Vintercepts <- vcs$Individual[1,1]
  Vslopes     <- vcs$Individual[2,2]
  corIS       <- cov2cor(vcs$Individual[,])[2]
  res         <- list(Vintercepts=Vintercepts, Vslopes=Vslopes, corIS=corIS)
  return(res)
}

Analyze <- function(df){

  d1   <- split(df, list(df$Replicate))
  res2 <- lapply(d1, lmerall)
  res3 <- matrix(unlist(res2), max(d$Replicate), 3, byrow=TRUE)
  return(res3)
}

Results <- function(x){
    ParameterEstimates <- data.frame(Par=c("Intercept", "slope", "Cor"), 
                                     Mean=round(apply(x, 2, mean),2)[1:3], 
                                     Q0.025=round(apply(x, 2, quantile, 0.025),2)[1:3], 
                                     Q0.975=round(apply(x, 2, quantile, 0.975),2)[1:3])
}

plotRS <- function(x, simvalues){
  for(i in 1:3){
      names <- c("Distribution of Vi estiamtes", "Distribution of Vs estiamtes", "Distribution of correlation estiamtes")
      hist(x[,i], main=names[i], ylim=c(0,60), breaks=10, xlab="Parameter estimates")
      abline(v=simvalues[i], lty=2, col="red")
  }
}

# par(mfcol=c(3,3))
# for(i in 1:3){
# d <- read.csv(dir[i])
# res1 <- Analyze(d)
# plotRS(res1, c(0.7, 0.1, 0.5))
# }
