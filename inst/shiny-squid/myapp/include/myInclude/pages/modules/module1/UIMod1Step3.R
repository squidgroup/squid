# UI: Module 1 Step 3

UIMod1Step3 <- function(){
  
  return(
    span( 
               
      h4(Mod1Step3_txt$title),          # Text: title
      
      p(HTML(Mod1Step3_txt$subgoal)),   # Text: subgoal
      p(HTML(Mod1Step3_txt$intro)),     # Text: introduction
      p(HTML(Mod1Step3_txt$exercise)),  # Text: exercise
      
      # Number of individuals
      # getNumericInput("Mod1Step3_NI", Modules_VAR$NI, "Mod1Step3_error_NI"),
      getSliderInput("Mod1Step3_NI", Modules_VAR$NI),
      
      # Among-individual variance (Vi)      
      # getNumericInput("Mod1Step3_Vi", Modules_VAR$Vi, "Mod1Step3_error_Vi"),
      getSliderInput("Mod1Step3_Vi", Modules_VAR$Vi),
      
      # Measurement error variance        
      # getNumericInput("Mod1Step3_Vme", Modules_VAR$Vme, "Mod1Step3_error_Vme"),
      getSliderInput("Mod1Step3_Vme", Modules_VAR$Vme),
      
      # Environment effects variance (Vx1)
      uiOutput("Mod1Step3_Vesu_txt"),
      uiOutput("Mod1Step3_error_Vesu"), 
      
      # Number of trait expressions (NR)
      getSliderInput("Mod1Step3_NR", Modules_VAR$Tmax),
      
      conditionalPanel(
        condition = "0",
        uiOutput("Mod1Step3_hidden")
      ),
      

      p(),
      # Simulation run button
      bsButton("Mod1Step3_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton", style = Modules_VAR$Run$style),
      runningIndicator(),
      p(),
      # Simulation progress bar
      
      # Output
        # Graph: density distribution of true values (Vp)
        #        density distribution of blups (Vi)
        #        density distribution of environnemental effect (Vx1)
        #        density distribution of deviation from blups (Vme)
        plotOutput("Mod1Step3_plot", width = Modules_VAR$Plot$width),
        # Scatter plot: measurements correlation
        plotOutput("Mod1Step3_plot2", width = Modules_VAR$Plot$width),
        
        # Table : display true and measured values (Vp, Vi, Vme and mean)  
        uiOutput("Mod1Step3_summary_table"),
      
      
      # Repeatability equation
      p(HTML(Mod1Step3_txt$para1)),         # Text: paragraph 1
      p(paste("$$Repeatability=\\frac{V'_",NOT$devI,"}{V'_",NOT$devI,"+V'_",NOT$residual,"}$$",sep="")),      
      textOutput("Mod1Step3_Rep_txt"),
      
      p(HTML(Mod1Step3_txt$point)),        # Text: point
      
      p(HTML(module1_txt$statModTitle)),
      p(paste("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",
              NOT$devI,"_",NOT$ind,"+",
              NOT$mean,"_{",NOT$specific,NOT$unknown,"}",
              NOT$env,"_{",NOT$specific,NOT$unknown,NOT$time,NOT$ind,"}+",
              NOT$error,"_{",NOT$time,NOT$ind,"}$$",sep="")),
      p(paste("$$V_",NOT$total,"=V_",NOT$devI,"+",general_VAR$EnvSpecUnk,"+V_",NOT$error,"$$",sep="")),
      
      p(HTML(Mod1Step3_txt$para2)),       # Text: paragraph 2
      
      div(class="line"),
      
      bsButton("Mod1Step3GotoStep2", label = "<< Previous Step (2)", style = Modules_VAR$StepLink$style), # Go to previous step       
      span("......", class="step-Link"),
      bsButton("Mod1Step3GotoStep4", label = "Next Step (4) >>", style = Modules_VAR$StepLink$style) # Go to next step
      
    
    )
  ) # End return
  
}