# UI: Module 6 Step 3
span(
    
    h4(Mod6Step3_txt$title),           # Text: title
    p(HTML(Mod6Step3_txt$subgoal)),    # Text: subgoal
    p(HTML(Mod6Step3_txt$intro)),      # Text: introduction
    p(HTML(Mod6Step3_txt$exercise)),   # Text: exercise
    
    p(paste0("$$",NOT$trait.1,"_{",NOT$time, NOT$ind,"}=
            ",EQ3$mean0,"+
             ",NOT$devI,"_",NOT$ind,"+
             (",NOT$mean,"+", NOT$devS,"_", NOT$ind,")", NOT$env,"_{",NOT$time, NOT$ind,"}+
             ",NOT$error,"_{",NOT$time, NOT$ind,"}$$")),
    
    displayRCode(Mod6Step3_txt$RCode),
    
    p(HTML(Mod6Step3_txt$table)),   # Text: table
    uiOutput("Mod6Step3_summary_variance_table"),
    
    p(HTML(Mod6Step3_txt$para1)),       # Text: paragraph 1
    p(HTML(Mod6Step3_txt$para2)),       # Text: paragraph 2
    
    selectInput("Mod6Step3_selector", "Number of total sampled observations:",
                c("100" = 100,
                  "225" = 225,
                  "400" = 400,
                  "900" = 900)),
   
   # Hidden variable:
    conditionalPanel(
      condition = "0",
      uiOutput("Mod6Step3_hidden")
    ),
    
    p(HTML(Mod6Step3_txt$para3)),       # Text: paragraph 3
    
    # Table 
    uiOutput("Mod6Step3_summary_table"),
    
     # Simulation run button
    actionButton("Mod6Step3_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
    runningIndicator(),
    sim_msg(),
   
    p(tags$b("Results")),
    
    p(plotOutput("Mod6Step3_plot", width = "700px")),
            
    p(HTML(Mod6Step3_txt$para4)),       # Text: paragraph 4
   
   div(class="line"),
   
   actionLink("Mod6Step3GotoStep2", label = "<< Previous Step (2)", class= "linkToModuleSteps") # Go to previous step      
)