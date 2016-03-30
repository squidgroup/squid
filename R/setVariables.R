# setVariables
#
# @description          test and initialize input variables
# 
# @param input          list of all the inputs used to run the model.
# @param module         character of the name of the module.
# @param environment    list of the model environments.
#
# @return               list of all model variables
#
setVariables <- function(input, module, environments, sep){
  
  ##############################################################
  ################## VARIABLES DECLARATION #####################
  ##############################################################  
  
  # Variables 
  Variables <- list(
    "nb.IS"    = 4,   # Minimum number of intercepts + slopes
    "nb.KE"    = 2,   # Number of known environment 
    "B0"       = 1,   # Position of intercept component in the (Co)variance matrix
    "X1"       = 2,   # Position of environment X1 component in the (Co)variance matrix
    "X2"       = 3,   # Position of environment X2 component in the (Co)variance matrix
    "X1X2"     = 4    # Position of environment interaction X1xX2 component in the (Co)variance matrix
  )
  
  inputNames <- list(
    "B"              = paste(module,"B"            , sep = sep),      
    "Vind"           = paste(module,"Vind"         , sep = sep),
    "Ve"             = paste(module,"Ve"           , sep = sep),
    "VG"             = paste(module,"VG"           , sep = sep),
    "Tmax"           = paste(module,"Tmax"         , sep = sep),      
    "Time_sampling"  = paste(module,"Time_sampling", sep = sep),
    "Vhsi"           = paste(module,"Vhsi"         , sep = sep),
    "NR_ind"         = paste(module,"NR_ind"       , sep = sep),
    "NR_trait"       = paste(module,"NR_trait"     , sep = sep),
    "ST_ind"         = paste(module,"ST_ind"       , sep = sep),
    "ST_trait"       = paste(module,"ST_trait"     , sep = sep),
    "NR_Fixe"        = paste(module,"NR_Fixe"      , sep = sep),
    "NP"             = paste(module,"NP"           , sep = sep),
    "NI"             = paste(module,"NI"           , sep = sep),
    "NT"             = paste(module,"NT"           , sep = sep),
    "NR"             = paste(module,"NR"           , sep = sep),
    "NG"             = paste(module,"NG"           , sep = sep),
    "PB"             = paste(module,"PB"           , sep = sep)
  )
  
  ##############################################################
  #################### INPUT VARIABLES  ########################
  ############################################################## 
  
  Mu   <- 0    # mean value of the normal distribution
  
  # Variance 
  V <- list(
    "Vp"     = 1,          # Totale variance value (Vp = Vind0 + Vind1 + Vt + Ve)              
    "Ve"     = ifelse(inputNames$Ve %in% names(input),input[[inputNames$Ve]],1e-10), # Measurement error variance
    "VG"     = ifelse(inputNames$VG %in% names(input),input[[inputNames$VG]],0) # Higher-level variance
  )
  
  x <- ifelse(inputNames$Tmax %in% names(input),input[[inputNames$Tmax]],1)
  
  if(inputNames$Time_sampling %in% names(input)){
    y <- input[[inputNames$Time_sampling]]
  }else{
    y <- c(1,x)
  }
  
  # Time 
  Time <- list(
    "Tmin"     = 1,                           # Start time
    "Tmax"     = x,                           # End time
    "TS"       = 1,                           # Time step value
    "Ts"       = y[1],                        # Start time of sampling
    "Te"       = y[2],                        # End time of sampling (Max = Tmax)
    "Tsamp"    = y[2]-y[1]+1,                 # Total sampling time
    "TsampI"   = 0,                           # Sampling time per individual
    
    "Vhsi"  = ifelse(inputNames$Vhsi %in% names(input),input[[inputNames$Vhsi]],0), # Among-individual variance in timing of sampling (between 0 and 1)
    
    "NR_ind"   = ifelse(inputNames$NR_ind    %in% names(input),input[[inputNames$NR_ind]],TRUE),            
    # Number of records among individuals
	# TRUE  : same number in records for all individuals
    # FALSE : different number of records among individuals
    "NR_trait" = ifelse(inputNames$NR_trait  %in% names(input),input[[inputNames$NR_trait]],TRUE),        
    # Number of records among traits within individuals
	# TRUE  : same number of records among traits within individuals
    # FALSE : different number of records among traits whitin individua    
    "ST_ind"   = ifelse(inputNames$ST_ind   %in% names(input),input[[inputNames$ST_ind]],TRUE),          
    # Sampling time among individuals
	# TRUE  : same sampling time among individuals
    # FALSE : different sampling time individuals
    "ST_trait" = ifelse(inputNames$ST_trait %in% names(input),input[[inputNames$ST_trait]],TRUE),     
    # Sampling time among traits within individuals
	# TRUE  : same sampling time among traits within individuals
    # FALSE : different sampling time among traits whitin individuals
    "NR_Fixe"  = ifelse(inputNames$NR_Fixe %in% names(input),input[[inputNames$NR_Fixe]],TRUE) # if TRUE the same NR for all the populations
  )
  
  Time$TsampI <- ifelse(Time$Vhsi > 0.95, round(Time$Tsamp*0.05,0), round(Time$Tsamp*(1-Time$Vhsi),0)) 
  Time$TsampI <- ifelse(Time$TsampI != 0, Time$TsampI, 1)
  
  N <- list(
    "NP"  = ifelse(inputNames$NP %in% names(input),input[[inputNames$NP]],1),      # Number of populations (between 1 and inf)
    "NI"  = ifelse(inputNames$NI %in% names(input),input[[inputNames$NI]],1),      # Number of individuals (between 2 and inf)
    "NT"  = ifelse(inputNames$NT %in% names(input),as.numeric(input[[inputNames$NT]]),1),   # Number of traits (between 1 and inf)
    "NS"  = (Time$Tmax - Time$Tmin + 1)/Time$TS,                                   # Number of step of time 
    "NR"  = ifelse(inputNames$NR %in% names(input),input[[inputNames$NR]],1),      # Number of mean records by individual (between 1 and inf) 
    "NRI" = NULL,                                                                  # Number of records for each individual
    "NG"  = ifelse(inputNames$NG %in% names(input),input[[inputNames$NG]],1)       # Number of high-level groups
  )
  
  # Vind0 : Random intercept Variance (among-individual variance)
  # Vind1 : Random slope variance 1   (within-individual variance)
  # Vind2 : Random slope variance 2   (within-individual variance)
  # Vind3 : Random slope variance 3   (within-individual variance)
  
  if(inputNames$Vind %in% names(input)){
    V$Vind <- as.matrix(input[[inputNames$Vind]])
    
        if(!environments$X1$state){
          V$Vind[seq(from=Variables$X1, to=(Variables$nb.IS*N$NT), by=Variables$nb.IS), ] <- 0 
          V$Vind[ ,seq(from=Variables$X1, to=(Variables$nb.IS*N$NT), by=Variables$nb.IS)] <- 0 
        }
        if(!environments$X2$state){
          V$Vind[seq(from=Variables$X2, to=(Variables$nb.IS*N$NT), by=Variables$nb.IS), ] <- 0 
          V$Vind[ ,seq(from=Variables$X2, to=(Variables$nb.IS*N$NT), by=Variables$nb.IS)] <- 0 
        }
        if(!environments$Interaction){
          V$Vind[seq(from=Variables$X1X2, to=(Variables$nb.IS*N$NT), by=Variables$nb.IS), ] <- 0 
          V$Vind[ ,seq(from=Variables$X1X2, to=(Variables$nb.IS*N$NT), by=Variables$nb.IS)] <- 0 
        }

  }else{
    V$Vind <- matrix(0,Variables$nb.IS, Variables$nb.IS)
  }
  
  # Population mean in the intercept and the slopes
  if(inputNames$B %in% names(input)){
    B <- matrix(input[[inputNames$B]], nrow=1)
    
    if(!environments$X1$state)    B[seq(from=Variables$X1, to=Variables$X1+(Variables$nb.IS*(N$NT-1)), by=Variables$nb.IS)]     <- 0 
    if(!environments$X2$state)    B[seq(from=Variables$X2, to=Variables$X2+(Variables$nb.IS*(N$NT-1)), by=Variables$nb.IS)]     <- 0 
    if(!environments$Interaction) B[seq(from=Variables$X1X2, to=Variables$X1X2+(Variables$nb.IS*(N$NT-1)), by=Variables$nb.IS)] <- 0 
    
    B <- repmat(as.matrix(B),N$NI*N$NS*N$NP, 1)
    B <- reshapeMat(B, Variables$nb.IS)
  }else{
    B <- matrix(0,N$NI*N$NS*N$NP*N$NT, Variables$nb.IS)
  }
  
  return(list("Mu"= Mu,"N" = N, "B" = B, "V" = V, "Time" = Time, "Variables" = Variables))
  
}