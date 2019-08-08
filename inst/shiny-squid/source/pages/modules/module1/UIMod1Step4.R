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
  getSliderInput("Mod1Step4_Ve", Modules_VAR$Vm),
  
  # Environment effects variance
  wellPanel(
    uiOutput("Mod1Step4_Vbx_txt"),
    uiOutput("Mod1Step4_error_Vbx")
  ),
  
  p("You can also set the number trait expressions "),
  # Number of trait expressions sampled
  getSliderInput("Mod1Step4_NR", Modules_VAR$NR),

  conditionalPanel(
    condition = "0",
    uiOutput("Mod1Step4_hidden")
  ),
  
  p(HTML(Mod1Step4_txt$para1)),    # Text: paragraph 1
  p(paste("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",
          EQ3$mean0, "+",
          NOT$devI,"_",NOT$ind,"+",
          NOT$mean," ",
          NOT$env,"_{",NOT$time,NOT$ind,"} +",
          NOT$error,"_{",NOT$time,NOT$ind,"}$$",sep="")),
  p(HTML(Mod1Step4_txt$para2)),      # Text: paragraph 2
  info_msg(Mod1Step4_txt$note1), # Text: note
  
  # Simulation run button
  actionButton("Mod1Step4_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
  runningIndicator(),
  sim_msg(),
  
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
          EQ3$mean0, "+",
          NOT$devI,"_",NOT$ind,"+",
          NOT$mean," ",
          NOT$env,"_{",NOT$time,NOT$ind,"} +",
          NOT$error,"_{",NOT$time,NOT$ind,"}$$",sep="")),
  p(paste("$$V_",NOT$total,"=V_",NOT$devI,"+V_{",NOT$mean," ",NOT$env,"}+V_",NOT$mError,"$$",sep="")),
  
  displayRCode(Mod1Step4_txt$RCode),
  
  div(class="line"),
  
  actionLink("Mod1Step4GotoStep3", label = "<< Previous Step (3)", class= "linkToModuleSteps") # Go to previous step
)