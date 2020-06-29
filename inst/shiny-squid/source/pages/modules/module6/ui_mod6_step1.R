# UI: Module 6 Step 1
span( 
  
    h4(Mod6Step1_txt$title),           # Text: title
    
    p(HTML(Mod6Step1_txt$subgoal)),    # Text: subgoal
    p(HTML(Mod6Step1_txt$intro)),      # Text: introduction
    p(HTML(Mod6Step1_txt$exercise)),    # Text: exercise
    
    p(paste0("$$",NOT$trait.1,"_{",NOT$time, NOT$ind,"}=
            ",EQ3$mean0,"+
            ",NOT$devI,"_",NOT$ind,"+
            ",NOT$mean," ",NOT$env,"_{",NOT$time, NOT$ind,"}+
            ",NOT$error,"_{",NOT$time, NOT$ind,"}$$")),

    displayRCode(Mod6Step1_txt$RCode1),
    
    p(HTML(Mod6Step1_txt$para1)),    # Text: paragraph 1
    
    p(paste0("$$",NOT$trait.1,"_{",NOT$time, NOT$ind,"}=
            ",EQ3$mean0,"+
            ",NOT$devI,"_",NOT$ind,"+
            (",NOT$mean,"+", NOT$devS,"_", NOT$ind,")", NOT$env,"_{",NOT$time, NOT$ind,"}+
            ",NOT$error,"_{",NOT$time, NOT$ind,"}$$")),
    
    displayRCode(Mod6Step1_txt$RCode2),
    
    p(HTML(Mod6Step1_txt$para2)),    # Text: paragraph 2
    
    # Number of individuals
    getSliderInput("Mod6Step1_NI", Modules_VAR$NI),
    
    # Number of trait expressions sampled
    getSliderInput("Mod6Step1_NR", Modules_VAR$NR),
    
    # Among-individual variance (Vi)
    fluidRow(
      column(8,getSliderInput("Mod6Step1_Vi", Modules_VAR$Vi)),
      column(4,textOutput("Mod6Step1_Vi_proportion", inline = TRUE))
    ),
    
    # Measurement error variance
    fluidRow(
      column(8,getSliderInput("Mod6Step1_Ve", Modules_VAR$Ve)),
      column(4,textOutput("Mod6Step1_Ve_proportion", inline = TRUE))
    ),
    div(info_msg(Mod6Step1_txt$note)),  # Text: note
    
    # Variance of Mean Environment effects in the slope (Vbx)
    fluidRow(
      column(8,getSliderInput("Mod6Step1_Vbx", Modules_VAR$Vbx)),
      column(4,textOutput("Mod6Step1_Vbx_proportion", inline = TRUE))
    ),

    #Individual-specific response to an environmental effect (random slopes) variance (VS)
    fluidRow(
      column(8,getSliderInput("Mod6Step1_Vs", Modules_VAR$Vsx)),
      column(4,textOutput("Mod6Step1_Vs_proportion", inline = TRUE))
    ),

    # Hidden variable:
    conditionalPanel(
      condition = "0",
      uiOutput("Mod6Step1_hidden")
    ),
    
    p(HTML(Mod6Step1_txt$para3)),    # Text: paragraph 3
    
    # Simulation run button
    actionButton("Mod6Step1_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
    runningIndicator(),
    sim_msg(),
    
    p(HTML(Mod6Step1_txt$results)),    # Text: results
    
    # Table : display true and measured values (Vp, Vi and mean)
    uiOutput("Mod6Step1_summary_table"),
    
    p(HTML(Mod6Step1_txt$para4)),    # Text: paragraph
    
    # Phenotype against environment figure
    p(plotOutput("Mod6Step1_plot", width = Modules_VAR$Plot$width)),
    
    p(HTML(Mod6Step1_txt$point)),   # Text: point
    
    p(HTML(module1_txt$statModTitle)),
    p(paste0("$$",NOT$trait.1,"_{",NOT$time, NOT$ind,"}=
            ",EQ3$mean0,"+
             ",NOT$devI,"_",NOT$ind,"+
             (",NOT$mean,"+", NOT$devS,"_", NOT$ind,")", NOT$env,"_{",NOT$time, NOT$ind,"}+
             ",NOT$error,"_{",NOT$time, NOT$ind,"}$$")),
    p(paste0("$$V_",NOT$total,"=
            V_",NOT$devI,"+
            V_{",NOT$mean,"}+
            V_{",NOT$devS,"}+
            V_",NOT$residualUpper,"$$")),
    
    p("where"),
    p(paste0("$$V_{",NOT$mean,"}=",NOT$mean,"^2Var(",NOT$env,")=",NOT$mean,"^2$$")),
    p(paste0("$$V_{",NOT$devS,"}=Var(",NOT$devS,")Var(",NOT$env,")+E(",NOT$env,")^2Var(",NOT$devS,")=Var(",NOT$devS,")$$")),
    p(paste0("Note that $Var(",NOT$env,")$ is the true variance in $",NOT$env,"$, and $E(",NOT$env,")$ is the true mean of $",NOT$env,"$.
             Also, in SQuID each environmental variable $(",NOT$env,")$ is standardized (i.e., $Var(",NOT$env,")=1$ and $E(",NOT$env,")=0$)$")),
    
    displayRCode(Mod6Step1_txt$RCode),
    
    div(class="line"),
    
    # Go to next step
    actionLink("Mod6Step1GotoStep2",
             label = "Next Step (2) >>",
             class= "linkToModuleSteps")
)