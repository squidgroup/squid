# UI: Module 4 Step 1
span( 
  
	# Text: title
  h4("Step 1: Phenotypic correlations between two repeatedly expressed traits 
  	 that are not consistently different between individuals."),
  
  # Sub-goal
  p(HTML("<b>Sub-goal:</b> Introducing within-individual correlations and variance-covariance matrices")), 
  
  # Introduction
	p(HTML("<b>Introduction:</b> As we have seen in other modules (Basic Lessons about Variance, 
  			 Random Regressions), repeatedly expressed traits can vary both within and among individuals. 
  			 In this module, we apply the same logic to correlations between traits. 
  			 We do this by introducing the notion that correlations also exist at multiple hierarchical levels. 
  			 Importantly, we explain how the correlations existing at each level influence the overall 
  			 phenotypic correlation in the data, and how their influence is mediated by the amount of 
  			 variation occurring at each level. We will introduce these ideas one step at the time; 
  			 we will start with focussing on two repeatedly expressed traits that do not vary among individuals. 
  			 In other words, those two traits only harbour within-individual variation.")),
  p(paste0("As a worked example, we consider two traits that we expect to be correlated because of trade-offs. 
  				 For example, trait y may represent egg size while trait z represents egg number (i.e., clutch size). 
  				 We expect trade-offs in the investment in these two traits because females producing large eggs 
  				 should typically have fewer resources available for producing a large clutch. 
  				 This means that we expect a negative correlation within the same individual parent 
  				 between egg size ($",NOT$trait.1,"$) and egg number ($",NOT$trait.2,"$).")),
  
  # Exercise
  p(HTML("<b>Exercise:</b> We will generate multi-level data for two repeatedly expressed traits. 
  			 We will consider a scenario where the two traits are assayed simultaneously for a set of individuals. 
  			 All individuals are sampled repeatedly but at the same time. We are assuming that you will sample 
  			 10 individuals with 10 samples per individual throughout.")),
  
  p("Set the amount of within-individual variance in each of the two traits:"),
	# Within-individual variances (Vi)
	fluidRow(
	  column(6,getSliderInput("Mod4Step1_Ve1", Modules_VAR$Ve1)),
	  column(6,getSliderInput("Mod4Step1_Ve2", Modules_VAR$Ve2))
	),
  
  p(paste0("Now set the within-individual correlation. Remember, for this example this 
  	correlation should be negative as we are considering trade-offs between 
  	egg size ($",NOT$trait.1,"$) and egg number ($",NOT$trait.2,"$).")),
  
  # Within-individual correlation
  getSliderInput("Mod4Step1_Corr_e", Modules_VAR$Corr_e),

  p(b("Results:")),
  p("Above, you have defined the elements of the within-individual “variance-covariance matrix”.  
  	This matrix holds the variances of the two traits on the diagonals 
  	and their covariance on the off-diagonals:")
  
  
  
  
  
    # 
    # displayRCode(Mod6Step1_txt$RCode1),
    # 
    # p(HTML(Mod6Step1_txt$para1)),    # Text: paragraph 1
    # 
    # p(paste0("$$",NOT$trait.1,"_{",NOT$time, NOT$ind,"}=
    #         ",EQ3$mean0,"+
    #         ",NOT$devI,"_",NOT$ind,"+
    #         (",NOT$mean,"+", NOT$devS,"_", NOT$ind,")", NOT$env,"_{",NOT$time, NOT$ind,"}+
    #         ",NOT$error,"_{",NOT$time, NOT$ind,"}$$")),
    # 
    # displayRCode(Mod6Step1_txt$RCode2),
    # 
    # p(HTML(Mod6Step1_txt$para2)),    # Text: paragraph 2
    # 
    # # Number of individuals
    # getSliderInput("Mod6Step1_NI", Modules_VAR$NI),
    # 
    # # Number of trait expressions sampled
    # getSliderInput("Mod6Step1_NR", Modules_VAR$NR),
    # 
    # # Among-individual variance (Vi)
    # fluidRow(
    #   column(8,getSliderInput("Mod6Step1_Vi", Modules_VAR$Vi)),
    #   column(4,textOutput("Mod6Step1_Vi_proportion", inline = TRUE))
    # ),
    # 
    # # Measurement error variance
    # fluidRow(
    #   column(8,getSliderInput("Mod6Step1_Ve", Modules_VAR$Ve)),
    #   column(4,textOutput("Mod6Step1_Ve_proportion", inline = TRUE))
    # ),
    # div(info_msg(Mod6Step1_txt$note)),  # Text: note
    # 
    # # Variance of Mean Environment effects in the slope (Vbx)
    # fluidRow(
    #   column(8,getSliderInput("Mod6Step1_Vbx", Modules_VAR$Vbx)),
    #   column(4,textOutput("Mod6Step1_Vbx_proportion", inline = TRUE))
    # ),
    # 
    # #Individual-specific response to an environmental effect (random slopes) variance (VS)
    # fluidRow(
    #   column(8,getSliderInput("Mod6Step1_Vs", Modules_VAR$Vsx)),
    #   column(4,textOutput("Mod6Step1_Vs_proportion", inline = TRUE))
    # ),
    # 
    # # Hidden variable:
    # conditionalPanel(
    #   condition = "0",
    #   uiOutput("Mod6Step1_hidden")
    # ),
    # 
    # (HTML(Mod6Step1_txt$para3)),    # Text: paragraph 3
    # 
    # p(),
    # # Simulation run button
    # actionButton("Mod6Step1_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
    # runningIndicator(),
    # p(),
    # 
    # (HTML(Mod6Step1_txt$results)),    # Text: results
    # 
    # # Table : display true and measured values (Vp, Vi and mean)
    # uiOutput("Mod6Step1_summary_table"),
    # 
    # (HTML(Mod6Step1_txt$para4)),    # Text: paragraph
    # 
    # # Phenotype against environment figure
    # p(plotOutput("Mod6Step1_plot", width = Modules_VAR$Plot$width)),
    # 
    # p(HTML(Mod6Step1_txt$point)),   # Text: point
    # 
    # p(HTML(module1_txt$statModTitle)),
    # p(paste0("$$",NOT$trait.1,"_{",NOT$time, NOT$ind,"}=
    #         ",EQ3$mean0,"+
    #          ",NOT$devI,"_",NOT$ind,"+
    #          (",NOT$mean,"+", NOT$devS,"_", NOT$ind,")", NOT$env,"_{",NOT$time, NOT$ind,"}+
    #          ",NOT$error,"_{",NOT$time, NOT$ind,"}$$")),
    # p(paste0("$$V_",NOT$total,"=
    #         V_",NOT$devI,"+
    #         V_{",NOT$mean," ",NOT$env,"}+
    #         V_{",NOT$devS,NOT$env,"}+
    #         V_",NOT$residualUpper,"$$")),
    # 
    # p("where"),
    # p(paste0("$$V_{",NOT$mean," ",NOT$env,"}=",NOT$mean,"^2Var(",NOT$env,")=",NOT$mean,"^2$$")),
    # p(paste0("$$V_{",NOT$devS,NOT$env,"}=Var(",NOT$devS,")Var(",NOT$env,")+E(",NOT$env,")^2Var(",NOT$devS,")=Var(",NOT$devS,")$$")),
    # p(paste0("Note that $Var(",NOT$env,")$ is the true variance in $",NOT$env,"$, and $E(",NOT$env,")$ is the true mean of $",NOT$env,"$.
    #          Also, in SQuID each environmental variable $(",NOT$env,")$ is standardized (i.e., $Var(",NOT$env,")=1$ and $E(",NOT$env,")=0$)$")),
    # 
    # displayRCode(Mod6Step1_txt$RCode),
    # 
    # div(class="line"),
    # 
    # # Go to next step
    # actionLink("Mod6Step1GotoStep2",
    #          label = "Next Step (2) >>",
    #          class= "linkToModuleSteps")
)