
# Model variables
nb.IS <- 4
B0    <- 1
X1    <- 2
X2    <- 3
X1X2  <- 4
 
FullModel_VAR <- list(
  
  # Plot
  "Plot"  = list( "width"      = "600px"
  ),
  
  # Run Button
  "Run"  = list( "label"        = "RUN",
                 "icon"         = icon("refresh"),
                 "style"        = "primary",
                 "invalidStyle" = "inverse",
                 "errorTxt"     = "Please make sure to fix all input errors before running the simulation."
  ),
  
  # ReRun Button
  "ReRun"  = list( "label"        = "re-RUN",
                   "icon"         = icon("refresh"),
                   "style"        = "default",
                   "invalidStyle" = "inverse",
                   "errorTxt"     = "Please make sure to fix all input errors before running the simulation."
  ),
  
  # Download sampled data Button
  "download_sampled"  = list( "label"     = "Download Sampled Data"),
  
  # Download raw data Button
  "download_raw"  = list( "label"     = "Download Raw Data"),
  
  # Download R code
  "download_Rcode"  = list( "label"     = "Download R code"),
  
  # save input Button
  "save"  = list( "label" = "Save"),
  
  # load input Button
  "load"  = list( "label" = "Load"),
  
  # Simulation time (Tmax)
  "Tmax"   = list( "label"       = "Time",
                   "infoTxt"     = "Simulation time (number of time steps).",
                   "value"       = 100,
                   "min"         = 1,
                   "max"         = "",
                   "step"        = 1,
                   "errorTxt"    = "Simulation time must be an integer superior or equal to 1."
  ),
  
  # Simulation time (Tmax)
  "NP"     = list( "label"       = "Replicates",
                   "infoTxt"     = "Number of replicates of the simulated world (distinct populations are generated with the same simulation design).",
                   "value"       = 1,
                   "min"         = 1,
                   "max"         = "",
                   "step"        = 1,
                   "errorTxt"    = "Number of replicates must be an integer superior or equal to 1."
  ),
  
  # Individual number per replicate
  "NI"     = list( "label"       = "Individual(s)",
                   "infoTxt"     = "Numbers of individuals generated within each population (replicate).",
                   "value"       = 5,
                   "min"         = 1,
                   "max"         = "",
                   "step"        = 1,
                   "errorTxt"    = "Number of individuals must be an integer superior or equal to 1.
                                    Number of individuals must also be divisible by the number of higher-level groups."
  ),
  
  # Number of trait per individual
  "NT"     = list( "label"       = "Trait(s)",
                   "infoTxt"     = "Number of trait(s) within each individual.",
                   "value"       = c("1" = 1,"2" = 2)
  ),
  
  "NTnames" = c(paste("$",NOT$trait.1,"$"),
                paste("$",NOT$trait.2,"$")),
  
  # Number of High-level groups
  "NG"  = list("label"       = "Higher-level groups",
               "infoTxt"     = "Number of higher-level groups. Number of groups must be lower than number of individuals. Number of individuals must be divisible by number of groups.",
               "value"       = 1,
               "min"         = 1,
               "max"         = "",
               "step"        = 1,
               "modulo"      = 5,
               "errorTxt"    = "Number of higher-level groups must be an integer 
                                between 1 and number of individuals.</br>
                                Number of individuals must be divisible by
                                the number of higher-level groups."
  ),
  
  # Environement variables 
  "Envnames" = c("X1", "X2", "EG", "ES"),
  
  "Env_preview" = paste0("In SQuID the final output of each environmental effect is standardized  
												 (i.e., $Var(",NOT$env,"=1)$ and $E(",NOT$env,")=0$).
  											 However, the previsualization graph below displays only the generated environmental data before standardization."),
  
  "X1"     = list( "state" = list("label" = "Add environment X1", 
                                  "infoTxt"  = "",
                                  "value" = FALSE),
                   "share" = list("label" = "Shared environment", 
                                  "infoTxt"  = "",
                                  "value" = TRUE)
  ),
  
  "X2"     = list( "state" = list("label" = "Add environment X2", 
                                  "infoTxt"  = "",
                                  "value" = FALSE),
                   "share" = list("label" = "Shared environment", 
                                  "infoTxt"  = "",
                                  "value" = TRUE)
  ),
  
  "X1X2"   = list( "state" = list("label" = "Add environment interaction", 
                                  "infoTxt"  = "",
                                  "value" = FALSE),
                   "info"  = "Interaction between environment X1 and X2 is 
                              available only when both environements are not null." 
  ),
  
  # Stochastic environmental effect
  "sto"     = list( "state" = list("label"    = "Add stochastic environment effect", 
                                  "infoTxt"   = "Generate a stochastic environmental effect following a normal distribution with mean 0 and variance that you could input below.",
                                  "value"     = FALSE),
                    "share" = list("label"    = "Shared environment", 
                                   "infoTxt"  = "If checked stochastic (and auto-correlated) environmental effect will be general to all individuals otherwise will be specific (different among individuals).",
                                   "value"    = TRUE)
  ),
  
  # Environemental effect variance (stochastic)
  "stoV"   = list( "label"       = "Variance",
                   "infoTxt"     = "Environmental effect variance of a normal distribution around mean 0.",
                   "value"       = 1,
                   "min"         = 0,
                   "max"         = "",
                   "step"        = 0.01,
                   "errorTxt"    = "Environmental effect variance must be a positive number."
  ),
  
  # Environemental autocorrelation effect 
  "sto_autocor_state" = list("label"    = "Add autocorrelation", 
                               "infoTxt"  = "Incorporate auto-correlation within the stochastic and normal environmental values.",
                               "value"    = FALSE
  ),
  
  # Environemental decay effect correlation
  "stoCorr"   = list(  "label"        = "Correlation",
                        "infoTxt"     = "Correlation value between two consecutive environmental values (note that a decay rate is applied on correlation values and will decreases them more the time between environmental values is large).",
                        "value"       = 0.5,
                        "min"         = 0,
                        "max"         = 1,
                        "step"        = 0.01,
                        "errorTxt"    = "Environemental decay effect (correlation) must be a positive proportion"
  ),
  
  # Linear environmental effect
  "lin"     = list( "state" = list("label"   = "Add linear environmental effect", 
                                   "infoTxt" = "Generate an environmental effect with linear trend.",
                                   "value"   = FALSE),
                    "share" = list("label"   = "Shared environment", 
                                   "infoTxt" = "If checked linear environmental effect will be general to all individuals otherwise will be specific (different among individuals).",
                                   "value"   = TRUE)
  ),
  
  # Linear environemental effect intercept
  "linI"   = list( "label"      = "Intercept",
                   "infoTxt"    = "Environmental effect intercept.",
                   "value"       = 0,
                   "min"         = "",
                   "max"         = "",
                   "step"        = 0.01,
                   "errorTxt"    = "Environmental effect intercept must be a number."
  ),
  
  # Linear environemental effect slope
  "linS"   = list( "label"       = "Slope",
                   "infoTxt"     = "Environmental effect slope.",
                   "value"       = 0.1,
                   "min"         = "",
                   "max"         = "",
                   "step"        = 0.01,
                   "errorTxt"    = "Environmental effect slope must be a number."
  ),
  
  
  # Cyclic environmental effect
  "cyc"     = list( "state" = list("label"   = "Add cyclic environmental effect", 
                                   "infoTxt" = "Generate an environmental effect with a cyclic pattern.",
                                   "value"   = FALSE),
                    "share" = list("label"   = "Shared environment", 
                                   "infoTxt" = "If checked cyclic environmental effect will be general to all individuals otherwise will be specific (different among individuals).",
                                   "value"   = TRUE)
  ),
  
  # Cyclic environemental effect Amplitude
  "cycA"   = list( "label"       = "Amplitude",
                   "infoTxt"     = "Environmental effect amplitude.",
                   "value"       = 10,
                   "min"         = "",
                   "max"         = "",
                   "step"        = 0.01,
                   "errorTxt"    = "Environmental effect amplitude must be a number."
  ),
  
  # Cyclic environemental effect Period
  "cycP"   = list( "label"       = "Period",
                   "infoTxt"     = "Environmental effect period.",
                   "value"       = 25,
                   "min"         = "",
                   "max"         = "",
                   "step"        = 0.01,
                   "errorTxt"    = "Environmental effect period must be a number."
  ),
  
  # Cyclic environemental effect Horizontal shift
  "cycH"   = list( "label"       = "Horizontal shift",
                   "infoTxt"     = "Environmental effect horizontal shift.",
                   "value"       = 0,
                   "min"         = "",
                   "max"         = "",
                   "step"        = 0.01,
                   "errorTxt"    = "Environmental effect horizontal shift must be a number."
  ),
  
  # Cyclic environemental effect Vertical shift
  "cycV"   = list( "label"       = "Vertical shift",
                   "infoTxt"     = "Environmental effect vertical shift.",
                   "value"       = 0,
                   "min"         = "",
                   "max"         = "",
                   "step"        = 0.01,
                   "errorTxt"    = "Environmental effect vertical shift must be a number."
  ),
  
  
  # Residual variance (Ve)
  "Ve"   = list("label"       = "Residual variance",
                "infoTxt"     = "Variance of unaccounted effect on the phenotype.",
                "value"       = 0.05,
                "min"         = 0,
                "max"         = "",
                "step"        = 0.01,
                "errorTxt"    = "Residual variance must be a positive number."
  ),
  
  # Higher-level grouping variance (VG)
  "VG"  = list("label"        = "Higher-level grouping variance",
                "infoTxt"     = "Higher-level grouping variance.",
                "value"       = 0,
                "min"         = 0,
                "max"         = "",
                "step"        = 0.01,
                "errorTxt"    = "Higher-level grouping variance must be a positive number."
  ),
  
  # Number of trait expressions (NR)
  "NR"   = list( "label"       = "Records",
                 "infoTxt"     = "Mean number of records over the population.",
                 "value"       = 20,
                 "min"         = 1,
                 "max"         = 100,
                 "step"        = 1,
                 "errorTxt"    = "Number of records must be an integer between 1 and the sampling time length."
  ),
  
  # Sampling time length
  "SampTime"   = list( "label"       = "Sampling time length",
                       "infoTxt"     = "Sampling time length for each individual within the population (negatively function to the among-individual variance in timing of sampling).",
                       "value"       = "",                       
                       "errorTxt"    = ""
  ),
  
  # Among-individual variance in timing of sampling
  "Vhsi"   = list(  "label"      = "Among-individual variance in timing of sampling",
                    "infoTxt"   = "Among-individual variance in timing of sampling.",
                    "value"     = 0.0,
                    "min"       = 0.0,
                    "max"       = 0.95,
                    "step"      = 0.01,
                    "errorTxt"  = "Among-individual variance in timing of sampling must be a number between 0 and 0.95."
  ),
  
  # Sampling time length
  "SampDesign_preview"   = list( "label"       = "Preview of the sampling design",
                                 "infoTxt"     = "Preview of the sampling design defined by the user.",
                                 "value"       = "",                       
                                 "errorTxt"    = ""
  ),
  
  # Mean Environemental effect
  "B"      = list( "label"     = "Population mean values",
                   "infoTxt"   = "Population mean values (fixed intercepts and slopes for each trait).",
                   "value"     = matrix(rep(0,nb.IS),1),
                   "min"       = "",
                   "max"       = "" ,
                   "errorTxt"  = "Population mean values must be numbers."),
  
  "Bnames" = c(paste0("$",EQ$mean0.1,"$"), 
               paste0("$",EQ$mean1.1,"$"), 
               paste0("$",EQ$mean2.1,"$"),
               paste0("$",EQ$mean12.1,"$"),
               paste0("$",EQ$mean0.2,"$",sep=""), 
               paste0("$",EQ$mean1.2,"$"), 
               paste0("$",EQ$mean2.2,"$"),
               paste0("$",EQ$mean12.2,"$")),
  
  "Vind"   = list( "label"     = "Individual Variance/Correlation matrix",
                   "infoTxt"   = "Individual Variance/Correlation matrix. Variances are on the matrix diagonal and correlation values are below the matrix diagonal.",
                   "value"     = matrix(rep(0,(nb.IS)^2),nb.IS),
                   "min"       = -1,
                   "max"       = 1 ,
                   "diagmin"   = 0,
                   "diagmax"   = "",
                   "errorTxt"  = "text",
                   "errorTxt1" = "Variance values must be a positive number.",
                   "errorTxt2"  = "Correlation values must be a number between -1 and 1.") ,
  
  "Vindnames" = c(paste0("$",EQ2$dev0.1,"$"), 
                  paste0("$",EQ2$dev1.1,"$"), 
                  paste0("$",EQ2$dev2.1,"$"), 
                  paste0("$",EQ2$dev12.1,"$"),
                  paste0("$",EQ2$dev0.2,"$"), 
                  paste0("$",EQ2$dev1.2,"$"), 
                  paste0("$",EQ2$dev2.2,"$"), 
                  paste0("$",EQ2$dev12.2,"$")),
  
  "Checkbox_NbRecords" = list( "infoTxt" = "Number of records sampled."),

  "NR_ind"  = list( "label"     = "Same among individuals",
                      "infoTxt"   = "If checked: same number of records among individuals.",
                      "value"     = TRUE,
                      "errorTxt"  = "text"
    
  ),
  
  "NR_trait"  = list( "label"     = "Same among traits within individuals",
                      "infoTxt"   = "If checked: same number of records among traits within individuals.",
                      "value"     = TRUE,
                      "errorTxt"  = "text" 
                      
  ),

  "Checkbox_SamTime" = list( "infoTxt" = "Time when records are sampled."),
  
  "ST_ind"  = list( "label"     = "Same among individuals",
                    "infoTxt"   = "If checked: same sampling times among individuals.",
                    "value"     = TRUE,
                    "errorTxt"  = "text" 
                      
  ),
  
  "ST_trait"  = list( "label"    = "Same among traits within individuals",
                      "infoTxt"   = "If checked: same sampling times among traits within individuals.",
                      "value"     = TRUE,
                      "errorTxt"  = "text"
                       
  )
)

diag(FullModel_VAR$Vind$value)  <- c(0.5,0.1,0.1,0.1)