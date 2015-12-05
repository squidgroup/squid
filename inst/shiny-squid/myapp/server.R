
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

# Source included packages and functions
source("include/include.R")

# Initialisation ------------------------------------------

# Graphs
color <- list(
  "color1"  = "red",
  "color2"  =  "dodgerblue"
)

data_S      <- NULL
environment <- NULL
sliderMin   <- NULL
sliderMax   <- NULL
TmaxOld     <- 100
Test        <- NULL

isPreparing <- FALSE
isRunning   <- FALSE


# My WebServer  ----------------------------------------------------------------

shinyServer(function(input, output, session) {

  # Portal ----------------
  
  # Modules   ---------------------------------------------------------  
  SVRmodules(input, session)  
  
    # Module 1  ---------------------------------------------------------  
  
    # Module 1 Step 1 ---------------------------------------------------------  
      SVRMod1Step1(input, output, session, color)
    
    # Module 1 Step 2 ---------------------------------------------------------
      SVRMod1Step2(input, output, session, color)
    
    # Module 1 Step 3 ---------------------------------------------------------
      SVRMod1Step3(input, output, session, color)

    # Module 1 Step 4 ---------------------------------------------------------
      SVRMod1Step4(input, output, session, color)

  
  # Module 3  --------------------------------------------------------- 
  
    # Module 3 Step 1 ---------------------------------------------------------
      SVRMod3Step1(input, output, session, Modules_VAR, nb.IS, color)
  
    # Module 3 Step 2 ---------------------------------------------------------
      SVRMod3Step2(input, output, session, Modules_VAR, FullModel_VAR, nb.IS, color)
  
    # Module 3 Step 3 ---------------------------------------------------------
      SVRMod3Step3(input, output, session, Modules_VAR, FullModel_VAR, nb.IS, color)
  
  # Module 6  --------------------------------------------------------- 
  
    # Module 6 Step 1 ---------------------------------------------------------
      SVRMod6Step1(input, output, session, Modules_VAR, nb.IS, color)
  
    # Module 6 Step 2 ---------------------------------------------------------
      SVRMod6Step2(input, output, session, Modules_VAR, nb.IS, color)
  
    # Module 6 Step 3 ---------------------------------------------------------
      SVRMod6Step3(input, output, session, Modules_VAR, nb.IS, color)

  
  # Full model  (Step by Step)   ----------------
    SVRFullModel("FModSbyS", input, output, session)  
  
  # Full model  (express access) ----------------
    SVRFullModel("FMod", input, output, session)
  
  #     output$table_test <- renderTable({ 
  #       if(!is.null(Mod1Step3_output())){
  #         Mod1Step3_output()$data_S
  #       }else{
  #         data.frame(0)
  #       }
  #     })
})
