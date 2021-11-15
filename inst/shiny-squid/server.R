# My WebServer  ----------------------------------------------------------------

# Initialisation ------------------------------------------
source("./source/svr_source.R",local = TRUE)

shinyServer(function(input, output, session) {
  
  
  # Automatically stop a Shiny app when closing the browser tab
  session$onSessionEnded(stopApp)
  
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
  source("./source/server/modules/svr_modules.R",local = TRUE)

  # Module 1  ---------------------------------------------------------

  # Module 1 Step 1 -----------------------
  source("./source/server/modules/module1/svr_mod1_step_1.R",local = TRUE)

  # Module 1 Step 2 -----------------------
  source("./source/server/modules/module1/svr_mod1_step_2.R",local = TRUE)

  # Module 1 Step 3 -----------------------
  source("./source/server/modules/module1/svr_mod1_step_3.R",local = TRUE)

  # Module 1 Step 4 -----------------------
  source("./source/server/modules/module1/svr_mod1_step_4.R",local = TRUE)

  # Module 2  ---------------------------------------------------------

  # Module 2 Step 1 -----------------------
  source("./source/server/modules/module2/svr_mod2_step_1.R",local = TRUE)

  # # Module 2 Step 2 -----------------------
  source("./source/server/modules/module2/svr_mod2_step_2.R",local = TRUE)

  # # Module 2 Step 3 -----------------------
  source("./source/server/modules/module2/svr_mod2_step_3.R",local = TRUE)


  # Module 3  ---------------------------------------------------------

  # Module 3 Step 1 -----------------------
  source("./source/server/modules/module3/svr_mod3_step_1.R",local = TRUE)

  # Module 3 Step 2 -----------------------
  source("./source/server/modules/module3/svr_mod3_step_2.R",local = TRUE)

  # Module 3 Step 3 -----------------------
  source("./source/server/modules/module3/svr_mod3_step_3.R",local = TRUE)


  # Module 4  ---------------------------------------------------------

  # Module 4 Step 1 -----------------------
  source("./source/server/modules/module4/svr_mod4_step_1.R",local = TRUE)

  # # Module 4 Step 2 -----------------------
  source("./source/server/modules/module4/svr_mod4_step_2.R",local = TRUE)

  # # Module 4 Step 3 -----------------------
  source("./source/server/modules/module4/svr_mod4_step_3.R",local = TRUE)

  # # Module 4 Step 5 -----------------------
  source("./source/server/modules/module4/svr_mod4_step_5.R",local = TRUE)


  # Module 5  ---------------------------------------------------------

  # Module 5 Step 1 -----------------------
  source("./source/server/modules/module5/svr_mod5_step_1.R",local = TRUE)

  # # Module 5 Step 2 -----------------------
  source("./source/server/modules/module5/svr_mod5_step_2.R",local = TRUE)


  # Module 6  ---------------------------------------------------------

  # Module 6 Step 1 -----------------------
  source("./source/server/modules/module6/svr_mod6_step_1.R",local = TRUE)

  # Module 6 Step 2 -----------------------
  source("./source/server/modules/module6/svr_mod6_step_2.R",local = TRUE)

  # Module 6 Step 3 -----------------------
  source("./source/server/modules/module6/svr_mod6_step_3.R",local = TRUE)


  # Module 8  ---------------------------------------------------------

  # Module 6 Step 1 ---------------------------------------------------------
  source("./source/server/modules/module8/svr_mod8_step_1.R",local = TRUE)

  # Module 6 Step 2 ---------------------------------------------------------
  source("./source/server/modules/module8/svr_mod8_step_2.R",local = TRUE)


  # Full model  (Step by Step)   ----------------
  SVRFullModel("FModSbyS", input, output, session)

  # Full model  (express access) ----------------
  SVRFullModel("FMod", input, output, session)

  ##### For Debugging ####
  output$debug <- renderPrint({
    sessionInfo()
  })
  
})
