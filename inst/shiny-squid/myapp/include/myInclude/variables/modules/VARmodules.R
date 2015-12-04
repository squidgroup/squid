
# Model variables

Modules_VAR <- list(
  
  # Plot
  "Plot"  = list( "width"      = "600px"
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
  "Vi"  = list("label"       = paste("Among-individual variance ($V_",NOT$devI,"$):",sep=""),
               "infoTxt"     = "Among-individual variance (random intercepts).",
               "value"       = 0.7,
               "min"         = 0,
               "max"         = 1,
               "step"        = 0.01,
               "errorTxt"    = "Among-individual variance ($V_",NOT$devI,"$) must be a number between 0 and 1."
  ),
  
  # Individual-specific response to an environmental effect (random slopes) variance (VS)
  "Vs"     = list(  "label"       = paste0("Individual-specific response to an environmental effect (random slopes) variance ($V_",NOT$devS,"$): "),
                    "infoTxt"     = "Individual-specific response to an environmental effect (random slopes) variance",
                    "value"       = "",
                    "min"         = 0,
                    "max"         = 1,
                    "step"        = 0.01,
                    "errorTxt"    = ""
  ),
  
  # Measurement error variance (Vme)
  "Vme"  = list("label"       = paste("Measurement error variance ($V_",NOT$error,"$):",sep=""),
                "infoTxt"     = "Measurement error variance",
                "value"       = 0.05,
                "min"         = 0,
                "max"         = 1,
                "step"        = 0.01,
                "errorTxt"    = "Measurement error variance ($V_",NOT$error,"$) must be a number between 0 and 1."
  ),
  
  # Number of trait expressions (NR)
  "NR"   = list( "label"       = "Number of trait expressions sampled:",
                 "infoTxt"     = "Number of trait expressions sampled for each individual.",
                 "value"       = 2,
                 "min"         = 2,
                 "max"         = 100,
                 "step"        = 1,
                 "errorTxt"    = "Number of trait expressions sampled must be an integer between 2 and 100."
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
  
  # Specific and Unknown Environemental effect variance
  "Vesu"  = list(  "label"       = paste("Specific and unknown environmental effect variance ($", general_VAR$EnvSpecUnk,"$):",sep=""),
                   "infoTxt"     = "Specific (unshared among individuals) and unknown (unmeasured) environmental effect variance",
                   "value"       = "",
                   "min"         = 0,
                   "max"         = 1,
                   "step"        = 0.01,
                   "errorTxt"    = paste("Specific and unknown environmental effect variance ($",general_VAR$EnvSpecUnk,"$) must be a number between 0 and 1.
                                         Try to decrease other variances.",sep="")
  ),
  
  # known Environemental effect variance
  "Vesk"   = list(  "label"       = paste("Specific and known environmental effect variance ($",general_VAR$EnvSpecKno,"$): ",sep=""),
                     "infoTxt"     = "Specific (unshared among individuals) and known (measured) environmental effect variance",
                     "value"       = "",
                     "min"         = 0,
                     "max"         = 1,
                     "step"        = 0.01,
                     "errorTxt"    = paste("Specific and known environmental effect variance ($",general_VAR$EnvSpecKno,"$) must be a number between 0 and 1.
                                         Try to decrease other variances.",sep="")
  ),
  
  # General and unknown environemental effect variance
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
  
  # Mean Environemental effect
  "B1"   = list(  "label"        = paste0("Mean environmental effect ($",EQ3$mean1,"$):"),
                   "infoTxt"     = "Mean effect of specific (unshared among individuals)and known (measured) environmental effect",
                   "value"       = 0,
                   "min"         = 0,
                   "max"         = 1,
                   "step"        = 0.01,
                   "errorTxt"    = "Mean effect of measured environment (beta ES) must be a number."
  ),
  
  # Among-individual variance in timing of sampling
  "Vit"   = list(  "label"       = "Among-individual variance in timing of sampling:",
                   "infoTxt"     = "Among-individual variance in timing of sampling.",
                   "value"       = 0,
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
                      "value"       = c("Random" = "ran",
                                        "Autocorrelated" = "auto",
                                        "Linear" = "lin",
                                        "Cyclic" = "cyc")
  )
  
)


