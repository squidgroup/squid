# UI: Module 1 Step 1
span(
    
  #### Title ####
  h4(Mod1Step1_txt$title),           # Text: title
  
  #### Subgoal ####
  p(HTML(Mod1Step1_txt$subgoal)),    # Text: subgoal
  #### Introduction ####
  p(HTML(Mod1Step1_txt$intro)),      # Text: introduction
  #### Exercice ####
  p(HTML(Mod1Step1_txt$exercise)),   # Text: exercise
  
  # Number of individuals
  getSliderInput("Mod1Step1_NI", Modules_VAR$NI),
  
  p(HTML(Mod1Step1_txt$para1)),        # Text: Paragraph 1
  p(HTML(Mod1Step1_txt$explanation1)), # Text: Notation explanation
  div(info_msg(Mod1Step1_txt$note1)),  # Text: note
  
  # Measurement error variance
  getSliderInput("Mod1Step1_Ve", Modules_VAR$Vm),
  
  # Hidden variable:
    # Mod1Step1_Vind: Intercepts and slopes (Co)variance matrix
  conditionalPanel(
    condition = "0",
    uiOutput("Mod1Step1_hidden")
  ),
  
  #### Run 1 ####
  actionButton("Mod1Step1_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
  runningIndicator(), # Simulation progress bar
  sim_msg(),
  
  #### Figure 1 ####
  # Graph: density distribution of true and measured phenotypic values
  plotOutput("Mod1Step1_plot", width = Modules_VAR$Plot$width),
  
  #### Table 1 ####
  # Table : display true and measured values (Vp, Vi and mean)
  uiOutput("Mod1Step1_summary_table"),
  
  #### Point ####
  p(HTML(Mod1Step1_txt$point)),       # Text: point
  #### Solution ####
  p(HTML(Mod1Step1_txt$solutions)),   # Text: solutions
  
  #### Statistical model ####
  p(HTML(module1_txt$statModTitle)),
  p(HTML(Mod1Step1_txt$statmodel1)),        # Text: statistical model paragraph 1
  #### Eqaution 1 ####
  p(paste("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",NOT$devI,"_",NOT$ind,"+",NOT$error,"_{",NOT$time,NOT$ind,"}$$",sep="")),
  p(HTML(Mod1Step1_txt$statmodel2)),        # Text: statistical model paragraph 2

  p(HTML(Mod1Step1_txt$statmodel3)),        # Text: statistical model paragraph 3
  #### Eqaution 2 ####
  p(paste("$$V_p=V_",NOT$devI,"+V_",NOT$mError,"$$",sep="")),
  
  div(class="line"),
  
  # Go to next step
  actionLink("Mod1Step1GotoStep2", label = "Next Step (2) >>", class= "linkToModuleSteps")
)