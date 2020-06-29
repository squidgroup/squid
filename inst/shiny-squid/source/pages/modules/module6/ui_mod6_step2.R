# UI: Module 6 Step 2
span(
    
    h4(Mod6Step2_txt$title),           # Text: title
    
    p(HTML(Mod6Step2_txt$subgoal)),    # Text: subgoal
    p(HTML(Mod6Step2_txt$intro)),      # Text: introduction
    p(HTML(Mod6Step2_txt$para1)),      # Text: paragraph 1
    
    p(paste0("$$",NOT$trait.1,"_{",NOT$time, NOT$ind,"}=
            ",EQ3$mean0,"+
            ",NOT$devI,"_",NOT$ind,"+
            (",NOT$mean,"+", NOT$devS,"_", NOT$ind,")", NOT$env,"_{",NOT$time, NOT$ind,"}+
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
      Var(", NOT$devI,")                 & Cov(", NOT$devI, ",", NOT$devS ,") \\\\
      Cov(", NOT$devI, ",", NOT$devS ,") & Var(", NOT$devS, ")\\\\
      \\end{pmatrix} 
      $$")),
    
    p(HTML(Mod6Step2_txt$para3)),      # Text: paragraph 3
    p(HTML(Mod6Step2_txt$exercise)),    # Text: exercise
    
    p(paste0("$$Cor(",NOT$devI,",",NOT$devS,")=
             \\frac{Cov(",NOT$devI,",",NOT$devS,")}
                   {\\sqrt{Var(",NOT$devI,")","Var(",NOT$devS,")}}$$")),
    
    p(HTML(Mod6Step2_txt$para4)),      # Text: paragraph 4
    
    # Number of individuals               
    getSliderInput("Mod6Step2_NI", Modules_VAR$NI),
    
    # Number of trait expressions sampled
    getSliderInput("Mod6Step2_NR", Modules_VAR$NR),
    
    # Among-individual variance (Vi)
    fluidRow(
      column(8,getSliderInput("Mod6Step2_Vi", Modules_VAR$Vi)),
      column(4,textOutput("Mod6Step2_Vi_proportion", inline = TRUE))
    ),
    
    # Measurement error variance
    fluidRow(
      column(8,getSliderInput("Mod6Step2_Ve", Modules_VAR$Ve)),
      column(4,textOutput("Mod6Step2_Ve_proportion", inline = TRUE))
    ),
    
    # Variance of Mean Environment effects in the slope (Vbx)
    fluidRow(
      column(8,getSliderInput("Mod6Step2_Vbx", Modules_VAR$Vbx)),
      column(4,textOutput("Mod6Step2_Vbx_proportion", inline = TRUE))
    ),
    
    #Individual-specific response to an environmental effect (random slopes) variance (Vs)
    fluidRow(
      column(8,getSliderInput("Mod6Step2_Vs", Modules_VAR$Vsx)),
      column(4,textOutput("Mod6Step2_Vs_proportion", inline = TRUE))
    ),
    
    # Correlation between I and S (CorIS ):
    getSliderInput("Mod6Step2_CorIS", Modules_VAR$CorIS),
    
    # Hidden variable:
    conditionalPanel(
      condition = "0",
      uiOutput("Mod6Step2_hidden")
    ),
    
    # Simulation run button
    actionButton("Mod6Step2_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
    runningIndicator(),
    sim_msg(),
    
    p(HTML(Mod6Step2_txt$results)),    # Text: results
    
    # Table : display true and measured values (Vp, Vi and mean)
    uiOutput("Mod6Step2_summary_table"),
    div(info_msg(Mod6Step2_txt$note1)),  # Text: note 1
    
    (HTML(Mod6Step2_txt$para5)),       # Text: paragraph 5
    
    p(plotOutput("Mod6Step2_plot", width = "700px")),
    
    p(HTML(Mod6Step2_txt$para6)),       # Text: paragraph 6
    p(HTML(Mod6Step2_txt$point)),       # Text: paragraph point
    
    p(paste0("$$V_",NOT$total,"=
            V_",NOT$devI,"+
            V_{",NOT$mean,"}+
            V_{",NOT$devS,"}+
            2COV_{",NOT$devI,",",NOT$devS,"}+
            V_",NOT$residualUpper,"$$")),
    p("where"),
    p(paste0("$$COV_{",NOT$devI,",",NOT$devS,"}=
             E(",NOT$env,")Cov(",NOT$devI,",",NOT$devS,")$$")),
    div(info_msg(Mod6Step2_txt$note2)),  # Text: note 2
    
    p(HTML(Mod6Step2_txt$para7)),        # Text: paragraph 7
    
    displayRCode(Mod6Step2_txt$RCode),
    
    div(class="line"),
    
    actionLink("Mod6Step2GotoStep1", label = "<< Previous Step (1)", class= "linkToModuleSteps"), # Go to previous step       
    span(Modules_VAR$StepLink$sep, class="step-Link"),
    actionLink("Mod6Step2GotoStep3", label = "Next Step (3) >>", class= "linkToModuleSteps") # Go to next step
)