# My WebServer  ----------------------------------------------------------------

# Initialisation ------------------------------------------
source("./source/SVRsource.R",local=TRUE)

shinyServer(function(input, output, session) {
  
  # Graphs
  color <- list(
    "color1"  = "red",
    "color2"  =  "dodgerblue"
  )
  
  isPreparing <- FALSE
  isRunning   <- FALSE
  
  # Portal ----------------
  
  # Modules   ---------------------------------------------------------  
  # SVRmodules(input, session)  
  source("./source/server/modules/SVRmodules.R",local=TRUE)
  
    # Module 1  ---------------------------------------------------------  
  
    # Module 1 Step 1 ---------------------------------------------------------  
      source("./source/server/modules/module1/SVRMod1Step1.R",local=TRUE)
    
    # Module 1 Step 2 ---------------------------------------------------------
      source("./source/server/modules/module1/SVRMod1Step2.R",local=TRUE)
    
    # Module 1 Step 3 ---------------------------------------------------------
      source("./source/server/modules/module1/SVRMod1Step3.R",local=TRUE)

    # Module 1 Step 4 ---------------------------------------------------------
      source("./source/server/modules/module1/SVRMod1Step4.R",local=TRUE)

  
  # Module 3  --------------------------------------------------------- 
  
    # Module 3 Step 1 ---------------------------------------------------------
      source("./source/server/modules/module3/SVRMod3Step1.R",local=TRUE)
  
    # Module 3 Step 2 ---------------------------------------------------------
      source("./source/server/modules/module3/SVRMod3Step2.R",local=TRUE)
  
    # Module 3 Step 3 ---------------------------------------------------------
      source("./source/server/modules/module3/SVRMod3Step3.R",local=TRUE)
  
  # Module 6  --------------------------------------------------------- 
  
    # Module 6 Step 1 ---------------------------------------------------------
      source("./source/server/modules/module6/SVRMod6Step1.R",local=TRUE)
  
    # Module 6 Step 2 ---------------------------------------------------------
      source("./source/server/modules/module6/SVRMod6Step2.R",local=TRUE)
  
    # Module 6 Step 3 ---------------------------------------------------------
      source("./source/server/modules/module6/SVRMod6Step3.R",local=TRUE)

  
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
