# UI: Module 3 Step 2
span(
  
  h4(Mod3Step2_txt$title),           # Text: title
  
  p(HTML(Mod3Step2_txt$subgoal)),    # Text: subgoal
  p(HTML(Mod3Step2_txt$intro)),      # Text: introduction
  p(HTML(Mod3Step2_txt$exercise)),   # Text: exercise
  
  # Among-individual variance (Vi)
  getSliderInput("Mod3Step2_Vi", Modules_VAR$Vi),

  # Measurement error variance
  getSliderInput("Mod3Step2_Ve", Modules_VAR$Ve),
  
  # Variance of Mean Environment effects in the slope (V Beta1 X1)
  getSliderInput("Mod3Step2_Vbx", Modules_VAR$Vb1x1),
  
  conditionalPanel(
    condition = "0",
    uiOutput("Mod3Step2_hidden")
  ), 
  
  p(HTML(Mod3Step2_txt$para1)),      # Text: paragraph 1
  
  getEnvironmentInput(3,2),
  
  p(HTML(Mod3Step2_txt$para2)),      # Text: paragraph 2
  
  # inputs
  getSliderInput("Mod3Step2_NR",  Modules_VAR$NR),
  getSliderInput("Mod3Step2_Vhsi", Modules_VAR$Vhsi),
  
  p(HTML(Mod3Step2_txt$para3)),      # Text: paragraph 3
  
  bsButton("Mod3Step2_previewPlot", label = Modules_VAR$Refresh$label, icon= Modules_VAR$Refresh$icon, style = Modules_VAR$Refresh$style),
  p(),
  plotOutput("Mod3Step2_previewPlot", width = Modules_VAR$Plot$width),
  
  p(),
  # Simulation run button
  bsButton("Mod3Step2_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton", style = Modules_VAR$Run$style),
  runningIndicator(),
  p(),

  p(HTML(Mod3Step2_txt$results)),    # Text: results
  
  p(HTML(Mod3Step2_txt$para4)),      # Text: paragraph 4
  
  p(paste0("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",NOT$devI,"_",NOT$ind,"+",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  p(HTML(Mod3Step2_txt$para5)),      # Text: paragraph 5
  
  uiOutput("Mod3Step2_summary_table"),
  
  p(HTML(Mod3Step2_txt$conclusion)), # Text: conclusion
  
  p(HTML(Mod3Step2_txt$para6)),      # Text: paragraph 6
  
  p(HTML(Mod3Step2_txt$para7)),      # Text: paragraph 7
  
  p(HTML(Mod3Step2_txt$para8)),      # Text: paragraph 8
  
  div(class="line"),
  
  bsButton("Mod3Step2GotoStep1", label = "<< Previous Step (1)", style = Modules_VAR$StepLink$style), # Go to previous step       
  span(Modules_VAR$StepLink$sep, class="step-Link"),
  bsButton("Mod3Step2GotoStep3", label = "Next Step (3) >>", style = Modules_VAR$StepLink$style) # Go to next step
)