# UI: Module 1 Step 4

UIMod1Step4 <- function(){
  
  return(
    span( 
               
      h4(Mod1Step4_txt$title),            # Text: title
      
      p(HTML(Mod1Step4_txt$subgoal)),     # Text: subgoal
      p(HTML(Mod1Step4_txt$intro)),       # Text: introduction
      p(HTML(Mod1Step4_txt$exercise)),    # Text: exercise
      
      #Number of individuals
      # getNumericInput("Mod1Step4_NI", Modules_VAR$NI, "Mod1Step4_error_NI"),
      getSliderInput("Mod1Step4_NI", Modules_VAR$NI),
      
      
      # Among-individual variance (Vi)      
      # getNumericInput("Mod1Step4_Vi", Modules_VAR$Vi, "Mod1Step4_error_Vi"),
      getSliderInput("Mod1Step4_Vi", Modules_VAR$Vi),
      
      # Measurement error variance (Vme)
      # getNumericInput("Mod1Step4_Vme", Modules_VAR$Vme, "Mod1Step4_error_Vme"),
      getSliderInput("Mod1Step4_Vme", Modules_VAR$Vme),
      
      # Environment effects variance
      wellPanel(
        uiOutput("Mod1Step4_Vesk_txt"),
        uiOutput("Mod1Step4_error_Vesk")
      ),
      
      # Number of trait expressions (NR)
      getSliderInput("Mod1Step4_NR", Modules_VAR$Tmax),

      conditionalPanel(
        condition = "0",
        uiOutput("Mod1Step4_hidden")
      ),
      
      p(HTML(Mod1Step4_txt$para1)),    # Text: paragraph 1
      p(paste("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",
              NOT$devI,"_",NOT$ind,"+",NOT$mean,"_{",NOT$specific,NOT$known,"}",
              NOT$env,"_{",NOT$specific,NOT$known,NOT$time,NOT$ind,"}+",
              NOT$error,"_{",NOT$time,NOT$ind,"}$$",sep="")),
      p(HTML(Mod1Step4_txt$para2)),    # Text: paragraph 2
      
      # Mean known environmental effect (Beta ES)
      # getNumericInput("Mod1Step4_Be1", Modules_VAR$Bes, "Mod1Step4_error_Be1"),
      
      p(),
      # Simulation run button
      bsButton("Mod1Step4_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton", style = Modules_VAR$Run$style),
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
      
      p(HTML(Mod1Step4_txt$point)),   # Text: point    
      
      p(HTML(module1_txt$statModTitle)),
      p(paste("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"}=",
              NOT$devI,"_",NOT$ind,"+",NOT$mean,"_{",NOT$specific,NOT$known,"}",
              NOT$env,"_{",NOT$specific,NOT$known,NOT$time,NOT$ind,"}+",
              NOT$error,"_{",NOT$time,NOT$ind,"}$$",sep="")),
      p(paste("$$V_",NOT$total,"=V_",NOT$devI,"+",general_VAR$EnvSpecKno,"+V_",NOT$error,"$$",sep="")),
      
      ####### TEST ##########
      tableOutput("table_test"),
      
      div(class="line"),
      
      bsButton("Mod1Step4GotoStep3", label = "<< Previous Step (3)", style="link") # Go to previous step      
    
    )
  ) # End return
  
}