# UI: Module 6 Step 2

UIMod6Step2 <- function(){
  
  return(
    span( 
        
        h4(Mod6Step2_txt$title),           # Text: title
        
        p(HTML(Mod6Step2_txt$subgoal)),    # Text: subgoal
        p(HTML(Mod6Step2_txt$intro)),      # Text: introduction
        p(HTML(Mod6Step2_txt$para1)),      # Text: paragraph 1
        
        p(paste0("$$",NOT$trait.1,"_",NOT$ind,"=
                (",EQ3$mean0,"+
                ",NOT$devI,"_",NOT$ind,")+
                (",EQ3$mean1,"+", NOT$devS,"_", NOT$ind,")", NOT$env,"_{",NOT$time, NOT$ind,"}+
                ",NOT$error,"_{",NOT$time, NOT$ind,"}$$")),
        
        p(HTML(Mod6Step2_txt$para2)),      # Text: paragraph 2
        
        p(paste0(
          "$$ \\begin{pmatrix}",
          NOT$devI,"_",NOT$ind , "\\\\",
          NOT$devS,"_",NOT$ind , "\\\\
          \\end{pmatrix}
          \\sim MNV(0,\\Omega_{",NOT$devI, NOT$devS,"}):
          \\Omega_{",NOT$devI, NOT$devS,"}=
          \\begin{pmatrix}
          V_{",   NOT$devI,"}                  &                        \\\\
          Cov_{", NOT$devI, ",", NOT$devS ,"} & V_{",   NOT$devS, "}\\\\
          \\end{pmatrix} 
          $$")),
        
        p(HTML(Mod6Step2_txt$para3)),      # Text: paragraph 3
        p(HTML(Mod6Step2_txt$exercise)),    # Text: exercise
        
        p(paste0("$$Cor_{",NOT$devI,",",NOT$devS,"}=
                 \\frac{Cov_{",NOT$devI,",",NOT$devS,"}}
                       {\\sqrt{V_",NOT$devI,"V_",NOT$devS,"}}$$")),
        
        p(HTML(Mod6Step2_txt$para4)),      # Text: paragraph 4
        
        # Number of individuals               
        getSliderInput("Mod6Step2_NI", Modules_VAR$NI),
        
        # Number of trait expressions sampled
        getSliderInput("Mod6Step2_NR", Modules_VAR$NR),
        
        # Among-individual variance (Vi) 
        getSliderInput("Mod6Step2_Vi", Modules_VAR$Vi),
        
        # Measurement error variance  
        getSliderInput("Mod6Step2_Vme", Modules_VAR$Vme),
        
        # Variance of Mean Environment effects in the slope (V Beta1 X1)
        getSliderInput("Mod6Step2_Vbx", Modules_VAR$Vb1x1),
        
        #Individual-specific response to an environmental effect (random slopes) variance (VS) :
        getSliderInput("Mod6Step2_Vs", Modules_VAR$Vs),
        
        # Correlation between I and S (CorIS ): 
        getSliderInput("Mod6Step2_CorIS", Modules_VAR$CorIS),
        
        # Hidden variable:
        conditionalPanel(
          condition = "0",
          uiOutput("Mod6Step2_hidden")
        ),
        
        p(),
        # Simulation run button
        bsButton("Mod6Step2_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton", style = Modules_VAR$Run$style),
        runningIndicator(),
        p(),
        
        p(HTML(Mod6Step2_txt$results)),    # Text: results
        
        # Table : display true and measured values (Vp, Vi and mean)      
        uiOutput("Mod6Step2_summary_table"),
        
        (HTML(Mod6Step2_txt$para5)),       # Text: paragraph 5

        p("graph 1"),
        p(plotOutput("Mod6Step2_plot", width = "700px")),
        
        p(HTML(Mod6Step2_txt$para6)),       # Text: paragraph 6
        p(HTML(Mod6Step2_txt$point)),       # Text: paragraph point
        
        p(paste0("$$V_",NOT$total,"=
                V_{",EQ3$mean1,NOT$env,"}+
                V_",NOT$devI,"+
                V_{",NOT$devS,NOT$env,"}+
                2Cov_{",NOT$devI,NOT$devS,NOT$env,"}+
                V_",NOT$error,"$$")),
        
        p(HTML(Mod6Step2_txt$para7)),        # Text: paragraph 7
        
        div(class="line"),
        
        bsButton("Mod6Step2GotoStep1", label = "<< Previous Step (1)", style = Modules_VAR$StepLink$style), # Go to previous step       
        span(Modules_VAR$StepLink$sep, class="step-Link"),
        bsButton("Mod6Step2GotoStep3", label = "Next Step (3) >>", style = Modules_VAR$StepLink$style) # Go to next step
        
    
    )
  ) # End return
  
}