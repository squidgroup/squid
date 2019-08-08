# UI: Module 3 Step 3
span(
  
  h4(Mod3Step3_txt$title),           # Text: title
  
  p(HTML(Mod3Step3_txt$subgoal)),    # Text: subgoal
  p(HTML(Mod3Step3_txt$intro)),      # Text: introduction
  p(HTML(Mod3Step3_txt$exercise1)),  # Text: exercise 1
  
  # Among-individual variance (Vi)
  fluidRow(
    column(8,getSliderInput("Mod3Step3_Vi", Modules_VAR$Vi)),
    column(4,textOutput("Mod3Step3_Vi_proportion", inline = TRUE))
  ),
  
  # Measurement error variance
  fluidRow(
    column(8,getSliderInput("Mod3Step3_Ve", Modules_VAR$Vm)),
    column(4,textOutput("Mod3Step3_Ve_proportion"))
  ),
  
  # Variance of Mean Environment effects in the slope (V Beta1 X1)
  fluidRow(
    column(8,getSliderInput("Mod3Step3_Vbx", Modules_VAR$VE)),
           column(4,textOutput("Mod3Step3_Vbx_proportion"))
    ),

  conditionalPanel(
    condition = "0",
    uiOutput("Mod3Step3_hidden")
  ), 
  
  p(HTML(Mod3Step3_txt$para1)),      # Text: paragraph 1
  
  getEnvironmentInput(3,3),
  
  p(HTML(Mod3Step3_txt$para2)),      # Text: paragraph 2
  
  getSliderInput("Mod3Step3_NR",  Modules_VAR$NR),
  getSliderInput("Mod3Step3_Vhsi", Modules_VAR$Vhsi),
  
  actionButton("Mod3Step3_previewPlot_btn", label = Modules_VAR$Refresh$label, icon= Modules_VAR$Refresh$icon, class="refreshButton"),
  sim_msg(),
  
  plotOutput("Mod3Step3_previewPlot", width = Modules_VAR$Plot$width),
  
  p(HTML(Mod3Step3_txt$para3)),      # Text: paragraph 3
  
  # Simulation run button
  actionButton("Mod3Step3_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
  runningIndicator(),
  sim_msg(),
  
  p(HTML(Mod3Step3_txt$results)),         # Text: results
  
  p(paste0("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",EQ3$mean0,"+",NOT$devI,"_",NOT$ind,"+",NOT$mean," ",NOT$env,"+",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  displayRCode(Mod3Step3_txt$RCode1),
  
  p(HTML(Mod3Step3_txt$para4)),      # Text: paragraph 4
  
  p(paste0("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",EQ3$mean0,"+",NOT$devI,"_",NOT$ind,"+",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  displayRCode(Mod3Step3_txt$RCode2),
  
  p(HTML(Mod3Step3_txt$para5)),      # Text: paragraph 5
  
  uiOutput("Mod3Step3_summary_table"),
  
  p(HTML(Mod3Step3_txt$para6)),      # Text: paragraph 6
  p(HTML(Mod3Step3_txt$reminder)),   # Text: notation reminder
  
  p(HTML(Mod3Step3_txt$exercise2)),  # Text: exercise 2
  
  sliderInput("Mod3Step3_Vbx_proportion",
              "Proportion of the environmental effect measured:",
              value = 0.2,
              min   = 0,
              max   = 1,
              step  = 0.01,
              width = "500px",
              post = ""
  ),
  
  p(HTML(Mod3Step3_txt$para8)),      # Text: paragraph 8
  
  getSliderInput("Mod3Step3_Vhsi2", Modules_VAR$Vhsi),
  
  # Simulation run button
  actionButton("Mod3Step3_Run2", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
  runningIndicator(),
  sim_msg(),
  
  p(HTML(Mod3Step3_txt$para9)),      # Text: paragraph 9
  
  uiOutput("Mod3Step3_summary_table_2"),
  
  p(HTML(Mod3Step3_txt$conclusion)), # Text: conclusion
  p(HTML(Mod3Step3_txt$conclusion2)), # Text: conclusion 2
  
  p(HTML(Mod3Step3_txt$finalcaveat)),# Text: a final caveat
  
  div(class="line"),
  
  actionLink("Mod3Step3GotoStep2", label = "<< Previous Step (2)", class= "linkToModuleSteps") # Go to previous step     
)