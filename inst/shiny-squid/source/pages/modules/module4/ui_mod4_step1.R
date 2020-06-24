# UI: Module 4 Step 1
span( 
  
	# Text: title
  h4("Step 1: Phenotypic correlations between two repeatedly expressed traits 
  	 that are not consistently different between individuals."),
  
  # Sub-goal
  p(HTML("<b>Sub-goal:</b> Introducing within-individual correlations and variance-covariance matrices")), 
  
  conditionalPanel(
    condition = "0",
    uiOutput("Mod4Step1_hidden")
  ),
  
  # Introduction
  p(HTML(paste0("<b>Introduction:</b> As we have seen in other modules (",Module_titles$mod1,", 
				 ",Module_titles$mod6,"), repeatedly expressed traits can vary both within and among individuals. 
  			 In this module, we apply the same logic to correlations between traits. 
  			 We do this by introducing the notion that correlations also exist at multiple hierarchical levels. 
  			 Importantly, we explain how the correlations existing at each level influence the overall 
  			 phenotypic correlation in the data, and how their influence is mediated by the amount of 
  			 variation occurring at each level. We will introduce these ideas one step at the time; 
  			 we will start with focussing on two repeatedly expressed traits that do not vary among individuals. 
  			 In other words, those two traits only harbour within-individual variation."))),
  
  p(paste0("As a worked example, we consider two traits that we expect to be correlated because of trade-offs. 
  				 For example, trait $",NOT$trait.1,"$ may represent egg size while trait $",NOT$trait.2,"$ represents 
					 egg number (i.e., clutch size). We expect trade-offs in the investment in these two traits because 
					 females producing large eggs should typically have fewer resources available for producing a large clutch. 
  				 This means that we expect a negative correlation within the same individual parent 
  				 between egg size ($",NOT$trait.1,"$) and egg number ($",NOT$trait.2,"$).")),
  
  # Exercise
  p(HTML("<b>Exercise:</b> We will generate multi-level data for two repeatedly expressed traits. 
  			 We will consider a scenario where the two traits are assayed simultaneously for a set of individuals. 
  			 All individuals are sampled repeatedly but at the same time. We are assuming that you will sample 
  			 10 individuals with 10 samples per individual throughout. In this exercise, we use the following models:")),
  
  p(paste0("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"} = ",NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,"}$$
  				  $$",NOT$trait.2,"_{",NOT$time,NOT$ind,"} = ",NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,"}$$")),
  
  p("Set the amount of within-individual variance in each of the two traits 
  	($V_{",NOT$error,"_",NOT$trait.1,"}$ and $V_{",NOT$error,"_",NOT$trait.2,"}$):"),
  # Within-individual variances (Ve)
  fluidRow(
    column(6,getSliderInput("Mod4Step1_Ve1", Modules_VAR$Ve1)),
    column(6,getSliderInput("Mod4Step1_Ve2", Modules_VAR$Ve2))
  ),
  
  p(paste0("Now set the within-individual correlation. Remember, for this example this 
  	correlation should be negative as we are considering trade-offs between 
  	egg size ($",NOT$trait.1,"$) and egg number ($",NOT$trait.2,"$).")),
  
  # Within-individual correlation
  getSliderInput("Mod4Step1_Corr_e", Modules_VAR$Corr_e),
  
  p(strong("Results:")),
  p('Above, you have defined the elements of the within-individual "variance-covariance matrix".  
  	This matrix holds the variances of the two traits on the diagonals and their covariance on the off-diagonals:'),
  
  p(paste0(
    "$$ \\Omega_{",NOT$error,"}=
		  	\\begin{pmatrix}
		  	V_{",NOT$error,"_",NOT$trait.1,"} & Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"} \\\\
		  	Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"} & V_{", NOT$error,"_",NOT$trait.2,"} \\\\
		  	\\end{pmatrix} 
		 $$")),
  
  p(paste0("Here, $V_{",NOT$error,"_",NOT$trait.1,"}$ is the within-individual variance in trait $",NOT$trait.1,"$, 
  				 $V_{",NOT$error,"_",NOT$trait.2,"}$ the within-individual variance in trait $",NOT$trait.1,"$, and 
  				 $Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$ the within-individual covariance between these two traits.")),
  
  p("The values that you have entered above result in the following variance covariance matrix:"),
  
  ######## Matrix with the values entered
  uiOutput("Mod4Step1_Covariance_Matrix"),
  
  p(paste0("Notably, you did not enter the covariance ($Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$) 
  				 between the two traits but rather their correlation. As you will learn later on in this module, 
  				 covariances are important for various calculations (see below), which is why we introduce them here. 
  				 The correlation is the covariance between the two traits when the associated variances are expressed 
  				 in standard deviation units. This therefore results in a standardized metric that can be compared 
  				 across samples differing in variance.")),
  
  p(paste0("The relationship between the within-individual correlation ($r_{",NOT$error,"_",NOT$trait.1,",",
           NOT$error,"_",NOT$trait.2,"}$) and covariance ($Cov_{",NOT$error,"_",NOT$trait.1,",",
           NOT$error,"_",NOT$trait.2,"}$) is:")),
  
  p(paste0("$$r_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}=
             \\frac{Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}}
  				 {\\sqrt{V_{",NOT$error,"_",NOT$trait.1,"}","V_{",NOT$error,"_",NOT$trait.2,"}}}$$")),
  
  p("Here is a plot of the simulated data showing the within-individual correlation, 
  	where each individual is given a different colour:"),
  
  # Simulation run button
  actionButton("Mod4Step1_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
  runningIndicator(),
  sim_msg(),
  
  ##### Plot correlation between two trait
  p(),
  plotOutput("Mod4Step1_correlationplot", width = Modules_VAR$Plot$width),
  
  p("Note that the relationship between the two traits should be negative if you did 
  	enter a negative correlation above, and that this relationship exists within individuals. 
  	That is, when a female (colour) increases egg size from one instance (e.g. year) to the next, 
  	she simultaneously decreases her egg number. In other words, the changes in one trait across 
  	repeated expressions of the same individual are associated with changes in another trait."),
  
  p(HTML("<b>Conclusion:</b> Variance-covariance matrices hold information on the amount of variance 
  			 existing in each trait as well as the covariances between them. 
  			 The correlations between two traits are calculated using this information 
  			 on variances and covariances as derived metrics.")),

    div(class = "line"),

    # Go to next step
    actionLink("Mod4Step1GotoStep2",
             label = "Next Step (2) >>",
             class = "linkToModuleSteps")
)