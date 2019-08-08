# UI: Module 1 Step 2
span( 
  
  h4(Mod1Step2_txt$title),           # Text: title
  
  p(HTML(Mod1Step2_txt$subgoal)),    # Text: subgoal
  p(HTML(Mod1Step2_txt$intro)),      # Text: introduction
  p(HTML(Mod1Step2_txt$exercise)),   # Text: exercise
  
  # Number of individuals
  getSliderInput("Mod1Step2_NI", Modules_VAR$NI),
  
  # Measurement error variance
  getSliderInput("Mod1Step2_Ve", Modules_VAR$Vm),
  
  p(HTML(Mod1Step2_txt$para1)),   # Text: paragraph 1
  
  # Number of trait expressions sampled
  getSliderInput("Mod1Step2_NR", Modules_VAR$NR),
  
  # Hidden variable:
  # Mod1Step2_Tmax: simulation time
  # Mod1Step2_Vind: Intercepts and slopes (Co)variance matrix
  conditionalPanel(
    condition = "0",
    uiOutput("Mod1Step2_hidden")
  ),
  
  # Simulation run button
  actionButton("Mod1Step2_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
  runningIndicator(),
  sim_msg(),
  
  p(HTML(Mod1Step2_txt$para2)),
  
  # Output
    # Graph: density distribution of true values (Vp)
    #        density distribution of blups (Vi)
    #        density distribution of deviation from blups (Vme)
    plotOutput("Mod1Step2_plot", width = Modules_VAR$Plot$width),
    p(HTML(Mod1Step2_txt$para3)),  # Text: paragraph 3
  
    # Table : display true and measured values (Vp, Vi, Vme and mean)
    p(HTML(Mod1Step4_txt$para3)),    # Text: statistical result
    uiOutput("Mod1Step2_summary_table"),
    p(HTML(Mod1Step2_txt$point)),  # Text: point
  
    # Repeatability equation
    p(HTML(Mod1Step2_txt$para4)),  # Text: paragraph 4
    p(withMathJax(paste("$$Repeatability=\\frac{V'_",NOT$devI,"}{V'_",NOT$devI,"+V'_",NOT$mError,"}$$",sep=""))),
    
    # Repeatability output
    textOutput("Mod1Step2_Rep_txt"),
    p(HTML(Mod1Step2_txt$para5)),  # Text: paragraph 5
  
    # Scatter plot: measurements correlation
    plotOutput("Mod1Step2_plot2", width = Modules_VAR$Plot$width),
    p(HTML(Mod1Step2_txt$para6)),  # Text: paragraph 6
  
  p(HTML(module1_txt$statModTitle)),
  p(paste("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",NOT$devI,"_",NOT$ind,"+",NOT$error,"_{",NOT$time,NOT$ind,"}$$",sep="")),
  p(paste("$$V_",NOT$total,"=V_",NOT$devI,"+V_",NOT$mError,"$$",sep="")),
  
  displayRCode(Mod1Step2_txt$RCode),
  
  div(class="line"),
  
  actionLink("Mod1Step2GotoStep1", label = "<< Previous Step (1)", class="linkToModuleSteps"), # Go to previous step       
  span(Modules_VAR$StepLink$sep, class="step-Link"),
  actionLink("Mod1Step2GotoStep3", label = "Next Step (3) >>", class="linkToModuleSteps") # Go to next step
)