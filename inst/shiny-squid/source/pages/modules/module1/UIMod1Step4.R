# UI: Module 1 Step 4
span(

  h4(Mod1Step4_txt$title),            # Text: title
  
  p(HTML(Mod1Step4_txt$subgoal)),     # Text: subgoal
  p(HTML(Mod1Step4_txt$intro)),       # Text: introduction
  p(HTML(Mod1Step4_txt$exercise)),    # Text: exercise
  
  #Number of individuals
  getSliderInput("Mod1Step4_NI", Modules_VAR$NI),
  
  # Among-individual variance (Vi)
  getSliderInput("Mod1Step4_Vi", Modules_VAR$Vi),
  
  # Measurement error variance (Ve)
  getSliderInput("Mod1Step4_Ve", Modules_VAR$Ve),
  
  # Environment effects variance
  wellPanel(
    uiOutput("Mod1Step4_Vbx_txt"),
    uiOutput("Mod1Step4_error_Vbx")
  ),
  
  # Number of trait expressions (NR)
  getSliderInput("Mod1Step4_NR", Modules_VAR$Tmax),

  conditionalPanel(
    condition = "0",
    uiOutput("Mod1Step4_hidden")
  ),
  
  p(HTML(Mod1Step4_txt$para1)),    # Text: paragraph 1
  p(paste("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",
          NOT$devI,"_",NOT$ind,"+",
          EQ3$mean1,
          EQ2$env1,"+",
          NOT$error,"_{",NOT$time,NOT$ind,"}$$",sep="")),
  p(HTML(Mod1Step4_txt$para2)),    # Text: paragraph 2
  
  p(),
  # Simulation run button
  actionButton("Mod1Step4_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
  runningIndicator(),
  p(),
  # Simulation progress bar
  
  # Output
    # Table : display true and measured values (Vp, Vme, mean and Beta es)  
    p(HTML(Mod1Step4_txt$para3)),    # Text: paragraph 3
    uiOutput("Mod1Step4_summary_table"),
  
    p(HTML(Mod1Step4_txt$para4)),   # Text: paragraph 4
    # Graph: Individual phenotypes over environment    
    plotOutput("Mod1Step4_plot1", width = Modules_VAR$Plot$width),
  
    p(HTML(Mod1Step4_txt$para5)),   # Text: paragraph 5
    # Graph: Individual reaction norm  
    plotOutput("Mod1Step4_plot2", width = Modules_VAR$Plot$width),
  
  p(HTML(Mod1Step4_txt$para6)),   # Text: paragraph 6
  p(HTML(Mod1Step4_txt$point)),   # Text: point
  
  p(HTML(module1_txt$statModTitle)),
  p(paste("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",
          NOT$devI,"_",NOT$ind,"+",EQ3$mean1,
          EQ2$env1,"+",
          NOT$error,"_{",NOT$time,NOT$ind,"}$$",sep="")),
  p(paste("$$V_",NOT$total,"=V_",NOT$devI,"+V_{",EQ3$mean1,EQ2$env1,"}+V_",NOT$error,"$$",sep="")),
  
  ####### TEST ##########
  tableOutput("table_test"),
  
  div(class="line"),
  
  actionLink("Mod1Step4GotoStep3", label = "<< Previous Step (3)", class= "linkToModuleSteps") # Go to previous step
)