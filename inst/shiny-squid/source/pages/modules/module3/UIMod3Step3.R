# UI: Module 3 Step 3
span(
  
  h4(Mod3Step3_txt$title),           # Text: title
  
  p(HTML(Mod3Step3_txt$subgoal)),    # Text: subgoal
  p(HTML(Mod3Step3_txt$intro)),      # Text: introduction
  p(HTML(Mod3Step3_txt$exercise)),   # Text: exercise
  
  # Among-individual variance (Vi)
  fluidRow(
    column(8,getSliderInput("Mod3Step3_Vi", Modules_VAR$Vi)),
    column(4,textOutput("Mod3Step3_Vi_proportion", inline = TRUE))
  ),
  
  # Measurement error variance
  fluidRow(
    column(8,getSliderInput("Mod3Step3_Ve", Modules_VAR$Ve)),
    column(4,textOutput("Mod3Step3_Ve_proportion"))
  ),
  
  # Variance of Mean Environment effects in the slope (V Beta1 X1)
  fluidRow(
    column(8,getSliderInput("Mod3Step3_Vbx", Modules_VAR$Vb1x1)),
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
  getSliderInput("Mod3Step3_Visj", Modules_VAR$Visj),
  
  p(),
  plotOutput("Mod3Step3_previewPlot", width = Modules_VAR$Plot$width),
  p(),
  
  p(HTML(Mod3Step3_txt$para3)),      # Text: paragraph 3
  
  p(),
  # Simulation run button
  bsButton("Mod3Step3_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton", style = Modules_VAR$Run$style),
  runningIndicator(),
  p(),
  
  p(HTML(Mod3Step3_txt$results)),         # Text: results
  
  p(paste0("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",NOT$devI,"_",NOT$ind,"+",EQ3$mean1,EQ$env1,"+",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  p(HTML(Mod3Step3_txt$para4)),      # Text: paragraph 4
  
  p(paste0("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",NOT$devI,"_",NOT$ind,"+",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  p(HTML(Mod3Step3_txt$para5)),      # Text: paragraph 5
  
  uiOutput("Mod3Step3_summary_table"),
  
  p(HTML(Mod3Step3_txt$para6)),      # Text: paragraph 6
  
  p(HTML(Mod3Step3_txt$para7)),      # Text: paragraph 7
  
  sliderInput("Mod3Step3_Vbx_proportion",
              "Proportion of the environmental effect measured:",
              value = Modules_VAR$Vx$value,
              min   = Modules_VAR$Vx$min,
              max   = Modules_VAR$Vx$max,
              step  = Modules_VAR$Vx$step,
              width = "500px",
              post = ""
  ),
  
  p(HTML(Mod3Step3_txt$para8)),      # Text: paragraph 8
  
  getSliderInput("Mod3Step3_Visj2", Modules_VAR$Visj),
  
  p(),
  # Simulation run button
  bsButton("Mod3Step3_Run2", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton", style = Modules_VAR$Run$style),
  runningIndicator(),
  p(),
  
  p(HTML(Mod3Step3_txt$para9)),      # Text: paragraph 9
  
  uiOutput("Mod3Step3_summary_table_2"),
  
  p(HTML(Mod3Step3_txt$conclusion)), # Text: conclusion
  p(HTML(Mod3Step3_txt$conclusion2)), # Text: conclusion 2
  
  p(HTML(Mod3Step3_txt$finalcaveat)),# Text: a final caveat
  
  div(class="line"),
  
  bsButton("Mod3Step3GotoStep2", label = "<< Previous Step (2)", style="link") # Go to previous step     
)