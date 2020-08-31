# Model notations
NOT <-list(
  
  "trait.1"        = "y",
  "trait.2"        = "z",
  "time"           = "h",
  "ind"            = "i",
  "group"          = "j",
  "mean"           = "\\beta",
  "devI"           = "I",
  "devS"           = "S",
  "env"            = "x",
  "envEffect"      = "E",
  "random"         = "S",
  "autocorrelated" = "A",
  "linear"         = "L",
  "cyclic"         = "C",
  "specific"       = "s",
  "general"        = "g",
  "known"          = "k",
  "unknown"        = "u",
  "groupV"         = "G",
  "error"          = "e",
  "mError"         = "m",
  "residual"       = "r",
  "residualUpper"  = "R",
  "total"          = "P",
  "repeatability"  = "R",
  "mu"             = "\\mu",
  "meanVector"     = "B"
)

# Model equation components
EQ <- list(
  
  "phen.1"       = paste(NOT$trait.1,"_{",NOT$time,NOT$ind,NOT$group,"}",spe=""),
  "phen.2"       = paste(NOT$trait.2,"_{",NOT$time,NOT$ind,NOT$group,"}",spe=""),
  
  "mean0.1"      = paste(NOT$mean,"_{0",NOT$trait.1,"}",sep=""),
  "mean0.2"      = paste(NOT$mean,"_{0",NOT$trait.2,"}",sep=""),
  "dev0.1"       = paste(NOT$devI,"_{",NOT$trait.1,NOT$ind,"}",sep=""),
  "dev0.2"       = paste(NOT$devI,"_{",NOT$trait.2,NOT$ind,"}",sep=""),
  
  "mean1.1"      = paste(NOT$mean,"_{1",NOT$trait.1,"}",sep=""),
  "mean1.2"      = paste(NOT$mean,"_{1",NOT$trait.2,"}",sep=""),
  "dev1.1"       = paste(NOT$devS,"_{1",NOT$trait.1,NOT$ind,"}",sep=""),
  "dev1.2"       = paste(NOT$devS,"_{1",NOT$trait.2,NOT$ind,"}",sep=""),
  "env1"         = paste(NOT$env,"_{1",NOT$time,NOT$ind,NOT$group,"}",sep=""),
  
  "mean2.1"      = paste(NOT$mean,"_{2",NOT$trait.1,"}",sep=""),
  "mean2.2"      = paste(NOT$mean,"_{2",NOT$trait.2,"}",sep=""),
  "dev2.1"       = paste(NOT$devS,"_{2",NOT$trait.1,NOT$ind,"}",sep=""),
  "dev2.2"       = paste(NOT$devS,"_{2",NOT$trait.2,NOT$ind,"}",sep=""),
  "env2"         = paste(NOT$env,"_{2",NOT$time,NOT$ind,NOT$group,"}",sep=""),
  
  "mean12.1"     = paste(NOT$mean,"_{12",NOT$trait.1,"}",sep=""),
  "mean12.2"     = paste(NOT$mean,"_{12",NOT$trait.2,"}",sep=""),
  "dev12.1"      = paste(NOT$devS,"_{12",NOT$trait.1,NOT$ind,"}",sep=""),
  "dev12.2"      = paste(NOT$devS,"_{12",NOT$trait.2,NOT$ind,"}",sep=""),
  "env12"        = paste(NOT$env,"_{1",NOT$time,NOT$ind,NOT$group,"}.",NOT$env,"_{2",NOT$time,NOT$ind,NOT$group,"}",sep=""),
  
  "group.1"      = paste(NOT$groupV,"_{",NOT$trait.1,NOT$group,"}",sep=""),
  "group.2"      = paste(NOT$groupV,"_{",NOT$trait.2,NOT$group,"}",sep=""),
  
  "error.1"      = paste(NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,NOT$group,"}",sep=""),
  "error.2"      = paste(NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,NOT$group,"}",sep="")
  
)

# Model components (without trait)
EQ1 <- list(
	
	"phen"       = paste(NOT$trait.1,"_{",NOT$time,NOT$ind,NOT$group,"}",spe=""),
	
	"mean0"      = paste(NOT$mean,"_0",sep=""),
	"dev0"       = paste(NOT$devI,"_",NOT$ind,sep=""),
	
	"mean1"      = paste(NOT$mean,"_1",sep=""),
	"dev1"       = paste(NOT$devS,"_{1",NOT$ind,"}",sep=""),
	"env1"       = paste(NOT$env,"_{1",NOT$time,NOT$ind,NOT$group,"}",sep=""),
	
	"mean2"      = paste(NOT$mean,"_2",sep=""),
	"dev2"       = paste(NOT$devS,"_{2",NOT$ind,"}",sep=""),
	"env2"       = paste(NOT$env,"_{2",NOT$time,NOT$ind,NOT$group,"}",sep=""),
	
	"mean12"     = paste(NOT$mean,"_{12}",sep=""),
	"dev12"      = paste(NOT$devS,"_{12",NOT$ind,"}",sep=""),
	"env12"      = paste(NOT$env,"_{1",NOT$time,NOT$ind,NOT$group,"}.",NOT$env,"_{2",NOT$time,NOT$ind,NOT$group,"}",sep=""),
	
	"group"      = paste(NOT$groupV,"_",NOT$group,sep=""),
	
	"error"      = paste(NOT$error,"_{",NOT$time,NOT$ind,NOT$group,"}",sep="")
	
)

# Model components (without indices: time, individual and group)
EQ2 <- list(

  "dev0.1"       = paste(NOT$devI,"_",NOT$trait.1,sep=""),
  "dev0.2"       = paste(NOT$devI,"_",NOT$trait.2,sep=""),
  
  "dev1.1"       = paste(NOT$devS,"_{1",NOT$trait.1,"}",sep=""),
  "dev1.2"       = paste(NOT$devS,"_{1",NOT$trait.2,"}",sep=""),
  "env1"         = paste(NOT$env,"_1",sep=""),
  
  "dev2.1"       = paste(NOT$devS,"_{2",NOT$trait.1,"}",sep=""),
  "dev2.2"       = paste(NOT$devS,"_{2",NOT$trait.2,"}",sep=""),
  "env2"         = paste(NOT$env,"_2",sep=""),
  
  "dev12.1"      = paste(NOT$devS,"_{12",NOT$trait.1,"}",sep=""),
  "dev12.2"      = paste(NOT$devS,"_{12",NOT$trait.2,"}",sep=""),
  "env12"        = paste(NOT$env,"_1",NOT$env,"_2",sep=""),
  
  "group.1"      = paste(NOT$groupV,"_",NOT$trait.1,sep=""),
  "group.2"      = paste(NOT$groupV,"_",NOT$trait.2,sep=""),
  
  "error.1"      = paste(NOT$error,"_",NOT$trait.1,sep=""),
  "error.2"      = paste(NOT$error,"_",NOT$trait.2,sep="")
  
)

# Model equation components (without indices: trait, time, individual and group)
EQ3 <- list(
  
  "mean0"     = paste(NOT$mean,"_0",sep=""),
  "dev0"      = NOT$devI,
  
  "mean1"     = paste(NOT$mean,"_1",sep=""),
  "dev1"      = paste(NOT$devS,"_1",sep=""),
  
  "mean2"     = paste(NOT$mean,"_2",sep=""),
  "dev2"      = paste(NOT$devS,"_2",sep=""),
  
  "mean12"    = paste(NOT$mean,"_{12}",sep=""),
  "dev12"     = paste(NOT$devS,"_{12}",sep="")

)

# Popular Variables
general_VAR <- list(
  
  "btwIndVarTimSamp"       = paste("V_{",NOT$time,"_s",NOT$ind,"}",sep=""), # Among individual variance in time sampling 
  "EnvSpecUnk"             = paste("V_{",NOT$specific,NOT$unknown,"}",sep=""),  # Variance of specific and unknown environmental effect 
  "EnvSpecKno"             = paste("V_{",NOT$specific,NOT$known,"}",sep=""),  # Variance of specific and known environmental effect 
  "EnvGenUnk"              = paste("V_{",NOT$general,NOT$unknown,"}",sep="")  # Variance of general and unknown environmental effect 
  
  )

outputNames <- c("Replicate", 
                 "Individual",
                 "Group",
                 "Individual_Trait",
                 "Trait", 
                 "Time",
                 "Phenotype",
                 "B0",
                 "B1",
                 "B2",
                 "B12",
                 "I",
                 "S1",
                 "S2",
                 "S12",
                 "X1",
                 "X2",
                 "X1X2",
                 "G",
                 "e"
)
