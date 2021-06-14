
# Model variables

Modules_VAR <- list(
  
  # Plot
  "Plot"       = list( "width"  = "600px"
  ),
  
  # Run Button
  "Run"  = list( "label"        = "RUN",
                 "icon"         = icon("refresh"),
                 "style"        = "primary",
                 "invalidStyle" = "inverse"
  ),
  
  # Refresh Button
  "Refresh"  = list( "label"        = "Refresh",
                     "icon"         = icon("refresh"),
                     "style"        = "default",
                     "invalidStyle" = "inverse"
  ),
  
  # Step links
  "StepLink"  = list( "iconR"  = icon("angle-double-right"),
                      "iconL"  = icon("angle-double-left"),
                      "sep"    = "......",
                      "style"  = "link"
  ),
  
  # Number of individual
  "NI"   = list( "label"       = "Number of individuals:",
                 "infoTxt"     = "Number of individuals sampled within the population.",
                 "value"       = 10,
                 "min"         = 2,
                 "max"         = 100,
                 "step"        = 1,
                 "errorTxt"    = "Number of individuals must be an integer between 2 and 100."
  ),

  # Among-individual variance (Vi)
  "Vi"  = list("label"       = paste0("Among-individual variance in intercept ($V_",NOT$devI,"$):"),
               "infoTxt"     = "Among-individual variance (random intercepts).",
               "value"       = 0.7,
               "min"         = 0,
               "max"         = 1,
               "step"        = 0.01,
               "errorTxt"    = "Among-individual variance ($V_",NOT$devI,"$) must be a number between 0 and 1."
  ),
  
  # Among-individual variance of the trait 1 (Vi)
  "Vi1"  = list("label"       = paste0("Among-individual variance in intercept for trait $",NOT$trait.1,"$ ($V_{",NOT$devI,"_",NOT$trait.1,"}$):"),
  						 "infoTxt"     = "Among-individual variance (random intercepts).",
  						 "value"       = 0.7,
  						 "min"         = 0,
  						 "max"         = 1,
  						 "step"        = 0.01,
  						 "errorTxt"    = ""
  ),
  # Among-individual variance of the trait 2 (Vi)
  "Vi2"  = list("label"       = paste0("Among-individual variance in intercept for trait $",NOT$trait.2,"$ ($V_{",NOT$devI,"_",NOT$trait.2,"}$):"),
  							"infoTxt"     = "Among-individual variance (random intercepts).",
  							"value"       = 0.7,
  							"min"         = 0,
  							"max"         = 1,
  							"step"        = 0.01,
  							"errorTxt"    = ""
  ),
  
  # Among-individual Correlation in the intercept
  "Corr_I"   = list("label"       = paste0("Among-individual correlation."),
  									"infoTxt"     = "",
  									"value"       = 0,
  									"min"         = -1,
  									"max"         = 1,
  									"step"        = 0.01,
  									"errorTxt"    = ""
  ),
  
  # Individual-specific response to an environmental effect (random slopes) variance (VS)
  "Vsx"    = list(  "label"       = paste0("Among-individual variance in slope to $",NOT$env,"$ ($V_{",NOT$devS,"}$): "),
                    "infoTxt"     = "Variance due to individual-specific responses to an environmental factor (random slopes)",
                    "value"       = 0.2,
                    "min"         = 0,
                    "max"         = 1,
                    "step"        = 0.01,
                    "errorTxt"    = ""
  ),
  
  # Individual-specific response to an environmental effect (random slopes) variance (VS)
  "Vsx.1"    = list(  "label"       = paste0("Among-individual variance in slope to $",EQ2$env1,"$ ($V_{",EQ3$dev1,"}$): "),
                    "infoTxt"     = "Variance due to individual-specific responses to an environmental factor (random slopes)",
                    "value"       = 0.2,
                    "min"         = 0,
                    "max"         = 1,
                    "step"        = 0.01,
                    "errorTxt"    = ""
  ),
  
  # Individual-specific response to an environmental effect (random slopes) variance (VS)
  "Vsx.2"    = list(  "label"       = paste0("Among-individual variance in slope to $",EQ2$env2,"$ ($V_{",EQ3$dev2,"}$): "),
                    "infoTxt"     = "Variance due to individual-specific responses to an environmental factor (random slopes)",
                    "value"       = 0.2,
                    "min"         = 0,
                    "max"         = 1,
                    "step"        = 0.01,
                    "errorTxt"    = ""
  ),
  
  # Individual-specific response to an environmental effect (random slopes) variance (VS)
  "Vsx.12"    = list("label"       = paste0("Among-individual variance in slope to $",EQ2$env12,"$ ($V_{",EQ3$dev12,"}$): "),
                    "infoTxt"     = "Variance due to individual-specific responses to an environmental factor (random slopes)",
                    "value"       = 0.2,
                    "min"         = 0,
                    "max"         = 1,
                    "step"        = 0.01,
                    "errorTxt"    = ""
  ),
  
  # Random intercept and slope correlation
  "CorIS" = list(  "label"        = paste0("Correlation between $",NOT$devI,"$ and $",NOT$devS,"$ ($Cor_{",NOT$devI,NOT$devS,"}$):"),
                    "infoTxt"     = "",
                    "value"       = 0,
                    "min"         = -1,
                    "max"         = 1,
                    "step"        = 0.01,
                    "errorTxt"    = ""
  ),
  
  # Random intercept and slope correlation
  "CorIS1" = list("label"       = paste0("Correlation between $",NOT$devI,"$ and $",EQ3$dev1,"$ ($Cor_{",NOT$devI,EQ3$dev1,"}$):"),
                  "infoTxt"     = "",
                  "value"       = 0,
                  "min"         = -1,
                  "max"         = 1,
                  "step"        = 0.01,
                  "errorTxt"    = ""
  ),
  
  # Random intercept and slope correlation
  "CorIS2" = list("label"       = paste0("Correlation between $",NOT$devI,"$ and $",EQ3$dev2,"$ ($Cor_{",NOT$devI,EQ3$dev2,"}$):"),
                  "infoTxt"     = "",
                  "value"       = 0,
                  "min"         = -1,
                  "max"         = 1,
                  "step"        = 0.01,
                  "errorTxt"    = ""
  ),
  
  # Random intercept and slope correlation
  "CorIS12" = list("label"       = paste0("Correlation between $",NOT$devI,"$ and $",EQ3$dev12,"$ ($Cor_{",NOT$devI,EQ3$dev12,"}$):"),
                  "infoTxt"     = "",
                  "value"       = 0,
                  "min"         = -1,
                  "max"         = 1,
                  "step"        = 0.01,
                  "errorTxt"    = ""
  ),
  
  # Random slope correlation
  "CorS1S2" = list("label"       = paste0("Correlation between $",EQ3$dev1,"$ and $",EQ3$dev2,"$ ($Cor_{",EQ3$dev1,EQ3$dev2,"}$):"),
                  "infoTxt"     = "",
                  "value"       = 0,
                  "min"         = -1,
                  "max"         = 1,
                  "step"        = 0.01,
                  "errorTxt"    = ""
  ),
  
  # Random slope correlation
  "CorS1S12" = list("label"      = paste0("Correlation between $",EQ3$dev1,"$ and $",EQ3$dev12,"$ ($Cor_{",EQ3$dev1,EQ3$dev12,"}$):"),
                   "infoTxt"     = "",
                   "value"       = 0,
                   "min"         = -1,
                   "max"         = 1,
                   "step"        = 0.01,
                   "errorTxt"    = ""
  ),
  
  # Random slope correlation
  "CorS2S12" = list("label"      = paste0("Correlation between $",EQ3$dev2,"$ and $",EQ3$dev12,"$ ($Cor_{",EQ3$dev2,EQ3$dev12,"}$):"),
                    "infoTxt"     = "",
                    "value"       = 0,
                    "min"         = -1,
                    "max"         = 1,
                    "step"        = 0.01,
                    "errorTxt"    = ""
  ),
  
  
  # Residual variance (Ve)
  "Ve"   = list("label"       = paste("Residual variance ($V_",NOT$residualUpper,"$):",sep=""),
                "infoTxt"     = "Variance of unaccounted effect on the phenotype. 
                                 Residual variance could include measurement error variance and/or unknown environmental effect variance.",
                "value"       = 0.05,
                "min"         = 0.01,
                "max"         = 1,
                "step"        = 0.01,
                "errorTxt"    = "Residual variance ($V_",NOT$mError,"$) must be a number between 0 and 1."
  ),
  # Residual variance (Ve)
  "Ve1"   = list("label"       = paste0("Within-individual variance trait $",NOT$trait.1,"$"),
  							 "infoTxt"     = "",
  							 "value"       = 0.05,
  							 "min"         = 0.01,
  							 "max"         = 1,
  							 "step"        = 0.01,
  							 "errorTxt"    = ""
  ),
  # Residual variance (Ve)
  "Ve2"   = list("label"       = paste0("Within-individual variance trait $",NOT$trait.2,"$"),
  							 "infoTxt"     = "",
  							 "value"       = 0.05,
  							 "min"         = 0.01,
  							 "max"         = 1,
  							 "step"        = 0.01,
  							 "errorTxt"    = ""
  ),
  # Residual Correlation
  "Corr_e"   = list("label"       = paste0("Within-individual correlation."),
	  							 "infoTxt"     = "",
	  							 "value"       = 0,
	  							 "min"         = -1,
	  							 "max"         = 1,
	  							 "step"        = 0.01,
	  							 "errorTxt"    = ""
  ),
  
  # measurement error variance
  "Vm"   = list("label"       = paste("Measurement error variance ($V_",NOT$mError,"$):",sep=""),
                "infoTxt"     = "Measurement error variance.",
                "value"       = 0.05,
                "min"         = 0.01,
                "max"         = 1,
                "step"        = 0.01,
                "errorTxt"    = "Measurement error variance ($V_",NOT$mError,"$) must be a number between 0 and 1."
  ),
  
  # Number of trait expressions (NR)
  "NR"   = list( "label"       = "Number of trait expressions:",
                 "infoTxt"     = "Number of measurements per individual.",
                 "value"       = 5,
                 "min"         = 1,
                 "max"         = 100,
                 "step"        = 1,
                 "errorTxt"    = "Number of trait expressions must be an integer between 1 and 100."
  ),
  
  # Simulation time (Tmax)
  "Tmax"   = list( "label"       = "Number of trait expressions:",
                   "infoTxt"     = "Number of trait expressions.",
                   "value"       = 2,
                   "min"         = 2,
                   "max"         = 100,
                   "step"        = 1,
                   "errorTxt"    = "Number of trait expressions must be an integer between 1 and 100."
  ),
  
  # Environmental effect variance 
  "VE"  = list(  "label"       = paste0("Environmental effect variance $(V_",NOT$envEffect,")$:"),
                  "infoTxt"     = "Environmental effect variance",
                  "value"       = 0.5,
                  "min"         = 0,
                  "max"         = 1,
                  "step"        = 0.01,
                  "errorTxt"    = "Environmental effect variance must be a positive number."
  ),
  
  # Variance of population mean response to an environmental effect x
  "Vbx"  = list(  "label"       = paste0("Variance due to population mean effect of an environmental factor $(V_{",NOT$mean,"})$:"),
                  "infoTxt"     = "Variance due to population mean effect of an environmental factor.",
                  "value"       = 0.5,
                  "min"         = 0,
                  "max"         = 1,
                  "step"        = 0.01,
                  "errorTxt"    = paste("Variance due to population mean effect of an environmental factor $(V_{",NOT$mean," ",NOT$env,"})$ 
                                       must be a number between 0 and 1.
                                       Try to decrease other variances.",sep="")
  ),
  
  # Variance of population mean response to an environmental effect x1
  "Vb1x1"  = list(  "label"       = paste("Variance of population mean response to an environmental effect $(V_{",EQ3$mean1," ",EQ2$env1,"})$:",sep=""),
                   "infoTxt"     = "Variance of population mean response to an environmental effect",
                   "value"       = 0.5,
                   "min"         = 0,
                   "max"         = 1,
                   "step"        = 0.01,
                   "errorTxt"    = paste("Variance of population mean response to an environmental effect ($V_{",EQ3$mean1," ",EQ2$env1,"}$) 
                                        must be a number between 0 and 1.
                                         Try to decrease other variances.",sep="")
  ),
  
  # Specific and known Environmental effect variance
  "Vesk"   = list(  "label"       = paste("Specific and known environmental effect variance ($",general_VAR$EnvSpecKno,"$): ",sep=""),
                     "infoTxt"     = "Specific (unshared among individuals) and known (measured) environmental effect variance",
                     "value"       = "",
                     "min"         = 0,
                     "max"         = 1,
                     "step"        = 0.01,
                     "errorTxt"    = paste("Specific and known environmental effect variance ($",general_VAR$EnvSpecKno,"$) must be a number between 0 and 1.
                                         Try to decrease other variances.",sep="")
  ),
  
  # General and unknown environmental effect variance
  "Vegu"   = list(  "label"       = paste("General and unknown environmental effect variance ($",general_VAR$EnvGenUnk,"$): ",sep=""),
                    "infoTxt"     = "General (unshared among individuals) and unknown (unmeasured) environmental effect variance",
                    "value"       = "",
                    "min"         = 0,
                    "max"         = 1,
                    "step"        = 0.01,
                    "errorTxt"    = paste("General and unknown environmental effect variance ($",general_VAR$EnvGenUnk,"$) must be a number between 0 and 1.
                                         Try to decrease other variances.",sep="")
  ),
  
  # Environmental effect variance
  "Vx"     = list(  "label"       = paste("Environmental effect variance ($V_",NOT$env,"$): ",sep=""),
                    "infoTxt"     = "Environmental effect variance",
                    "value"       = "",
                    "min"         = 0,
                    "max"         = 1,
                    "step"        = 0.01,
                    "errorTxt"    = paste("General and unknown environmental effect variance ($V_",NOT$env,"$) must be a number between 0 and 1.
                                         Try to decrease other variances.",sep="")
  ),
  
  # Mean Environmental effect
  "B0"   = list(  "label"        = paste0("Population mean effect ($",EQ3$mean0,"$):"),
                  "infoTxt"     = "",
                  "value"       = 0,
                  "min"         = -1,
                  "max"         = 1,
                  "step"        = 0.01,
                  "errorTxt"    = ""
  ),
  
  # Mean Environmental effect
  "B1"   = list(  "label"        = paste0("Population mean response to the environment $",NOT$env,"$ ($",NOT$mean,"$):"),
                  "infoTxt"     = "Population mean response to the environment.",
                  "value"       = 0,
                  "min"         = -1,
                  "max"         = 1,
                  "step"        = 0.01,
                  "errorTxt"    = paste0("Population mean response to the environment ($",NOT$mean,"$) must be a number.")
  ),
  
  
  
  # Mean Environemental effect
  "B1.1"   = list( "label"        = paste0("Mean environmental effect ($",EQ3$mean1,"$):"),
                   "infoTxt"     = "Population mean response to the environment.",
                   "value"       = 0,
                   "min"         = -1,
                   "max"         = 1,
                   "step"        = 0.01,
                   "errorTxt"    = ""
  ),
  
  "B2.1"   = list(  "label"        = paste0("Mean environmental effect ($",EQ3$mean2,"$):"),
  								"infoTxt"     = "Population mean response to the environment.",
  								"value"       = 0,
  								"min"         = -1,
  								"max"         = 1,
  								"step"        = 0.01,
  								"errorTxt"    = ""
  ),
 
  "B1122" = list(  "label"        = paste0("Mean environmental effect ($",EQ3$mean12,"$):"),
  								"infoTxt"     = "Population mean response to the environment.",
  								"value"       = 0,
  								"min"         = -1,
  								"max"         = 1,
  								"step"        = 0.01,
  								"errorTxt"    = ""
  ),
  
  "B11"   = list( "label"        = paste0("Population-level slope trait $",NOT$trait.1,"$ ($",EQ$mean1.1,"$):"),
  								"infoTxt"     = "Population mean response to the environment.",
  								"value"       = 0,
  								"min"         = -1,
  								"max"         = 1,
  								"step"        = 0.01,
  								"errorTxt"    = ""
  ),
  "B12"   = list(  "label"        = paste0("Population-level slope $",NOT$trait.2,"$ ($",EQ$mean1.2,"$):"),
  								 "infoTxt"     = "Population mean response to the environment.",
  								 "value"       = 0,
  								 "min"         = -1,
  								 "max"         = 1,
  								 "step"        = 0.01,
  								 "errorTxt"    = ""
  ),
  
  # Among-individual variance in timing of sampling
  "Vhsi"   = list( "label"       = "Among-individual variance in timing of sampling:",
                   "infoTxt"     = "Among-individual variance in timing of sampling.",
                   "value"       = 0.5,
                   "min"         = 0,
                   "max"         = 0.95,
                   "step"        = 0.01,
                   "errorTxt"    = "Among-individual variance in timing of sampling must be a number between 0 and 0.95."
  ),
  
  "share" = list("label"    = "Shared environment", 
                 "infoTxt"  = "(Shared) Individuals experience the same environment.",
                 "value"    = TRUE
  ),
  
  # Number of trait per individual
  "Env_types" = list( "label"       = "Environment types",
                      "infoTxt"     = "",
                      "value"       = c("Stochastic" = "sto",
                                        "Autocorrelated" = "auto",
                                        "Linear" = "lin",
                                        "Cyclic" = "cyc")
  )
)

Module_titles <- list(
  "mod1"  = "Basic Lessons about Variance",
  "mod2"  = "Non-Gaussian traits",
  "mod3"  = "Non-stochastic environments",
  "mod4"  = "Multiple traits",
  "mod5"  = "Multi-dimensional phenotypic plasticity",
  "mod6"  = "Random regressions",
  "mod7"  = "",
  "mod8"  = "MDPP and random slopes",
  "mod10" = ""
)


