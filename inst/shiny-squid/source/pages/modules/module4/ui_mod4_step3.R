# UI: Module 4 Step 3
span( 
  
	# Text: title
	h4("Step 3: Level-specific correlations."),
	
	# Sub-goal
	p(HTML("<b>Sub-goal:</b> Understanding the role of level-specific correlations in shaping 
				 phenotypic correlations in repeatedly expressed traits.")), 
	
	
	conditionalPanel(
		condition = "0",
		uiOutput("Mod4Step3_hidden")
	),
	
	# Introduction
	p(HTML(paste0("<b>Introduction:</b> As a final step in our analysis of the within-individual 
								trade-off between egg and clutch size, we will introduce another level of complexity. 
								We will consider the idea that correlations often differ between hierarchical levels 
								because of multiple mechanisms shaping phenotypic correlations. We will continue our example where 
								individuals have to allocate limited resources into multiple costly traits, causing negative within-individual 
								correlations due to trade-offs. We will now introduce the notion that there is a second 
								mechanism causing covariance, namely among-individual variation in the acquisition of resources. 
								This may lead to a situation where individuals with many resources are able to produce both large 
								eggs and large clutches whereas individuals with fewer resources produce small eggs 
								and small clutches instead. Variation in acquisition combined with allocation trade-offs 
								therefore will lead to level-specific associations (van Noordwijk & de Jong 1986). 
								In statistical terms, an individual's average egg size will correlate positively with 
								its average clutch size over all its expressions. Essentially, what we have now set up 
								is a situation where the correlations between egg size and number are opposite across hierarchical levels."))),
	
	# Exercise
	p(HTML("<b>Exercise:</b> Set the within- and among-individual variances for the two traits. 
				 We suggest that you create a dataset where the associated repeatabilities are 0.5 as this will 
				 result in a situation where the phenotypic correlation is affected equally by correlations 
				 at each of the two levels. You will also have to set the within- and among-individual correlation. 
				 We suggest that you start with setting them to the same value but with opposite signs 
				 (e.g. within: $r$=-0.5 vs. between $r$=+0.5). As in step 1, we have already set both the number of individuals, 
				 and the number of repeated measures per individual, to 10.")),
	
	p("Set the amount of within-individual variance in each of the two traits:"),
	# Within-individual variances (Ve)
	fluidRow(
		column(6,getSliderInput("Mod4Step3_Ve1", Modules_VAR$Ve1)),
		column(6,getSliderInput("Mod4Step3_Ve2", Modules_VAR$Ve2))
	),
	
	p("Set the within-individual correlation, which should be negative in case female 
		trade-off their investment in egg size versus number"),
	# Within-individual correlation
	getSliderInput("Mod4Step3_Corr_e", Modules_VAR$Corr_e),
	
	p("Set the amount of among-individual variance in each of the two traits:"),
	fluidRow(
		column(6,getSliderInput("Mod4Step3_Vi1", Modules_VAR$Vi1)),
		column(6,getSliderInput("Mod4Step3_Vi2", Modules_VAR$Vi2))
	),
	
	p("Set the among-individual correlation, which should be positive if females with more resources can 
		produce both large eggs and large clutches."),
	# Among-individual correlation
	getSliderInput("Mod4Step3_Corr_I", Modules_VAR$Corr_I),
	
	p(strong("Results:")),
	
	p('You have defined the elements of the within-individual "variance-covariance matrix":'),
	p(paste0(
		"$$ \\Omega_{",NOT$error,"}=
		\\begin{pmatrix}
		V_{",NOT$error,"_",NOT$trait.1,"} & Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}  \\\\
		Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"} & V_{", NOT$error,"_",NOT$trait.2,"} \\\\
		\\end{pmatrix} 
		$$")),
	
	p("Your entries resulted in the following values for this matrix:"),
	
	######## Matrix with the values entered
	uiOutput("Mod4Step3_Within_Covariance_Matrix"),
	
	p("You have now also defined the elements of the among-individual matrix:"),
	p(paste0(
		"$$ \\Omega_{",NOT$devI,"}=
		\\begin{pmatrix}
		V_{",NOT$devI,"_",NOT$trait.1,"} & Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}  \\\\
		Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"} & V_{", NOT$devI,"_",NOT$trait.2,"} \\\\
		\\end{pmatrix} 
		$$")),
	
	p("Your entries resulted in the following values for this matrix:"),
	
	######## Matrix with the values entered
	uiOutput("Mod4Step3_Among_Covariance_Matrix"),
	
	p("As you have set both the among- and within-individual variances, 
		you have set the repeatabilities of both traits. Those are:"),
	
	###### Repeatabilities OUTPUT  
	uiOutput("Mod4Step3_Repeatabilities"),
	
	p("We will now again return to the equation describing the components affecting the phenotypic correlation:"),
	
	####### eq 1
	p(paste0("$$",
	         "r_{",NOT$total,"_",NOT$trait.1,",",NOT$total,"_",NOT$trait.2,"} = 
	         
	         r_{",NOT$devI,"_" ,NOT$trait.1,",",NOT$devI,"_" ,NOT$trait.2,"}
	         \\sqrt{
	         (\\frac{V_{",NOT$devI,"_" ,NOT$trait.1,"}}
	         {V_{",NOT$devI,"_" ,NOT$trait.1,"} + V_{",NOT$error,"_" ,NOT$trait.1,"}})
	         (\\frac{V_{",NOT$devI,"_" ,NOT$trait.2,"}}
	         {V_{",NOT$devI,"_" ,NOT$trait.2,"} + V_{",NOT$error,"_" ,NOT$trait.2,"}})} + 
	         
	         r_{",NOT$error,"_" ,NOT$trait.1,",",NOT$error,"_" ,NOT$trait.2,"}
	         \\sqrt{
	         (\\frac{V_{",NOT$error,"_" ,NOT$trait.1,"}}
	         {V_{",NOT$devI,"_" ,NOT$trait.1,"} + V_{",NOT$error,"_" ,NOT$trait.1,"}})
	         (\\frac{V_{",NOT$error,"_" ,NOT$trait.2,"}}
	         {V_{",NOT$devI,"_" ,NOT$trait.2,"} + V_{",NOT$error,"_" ,NOT$trait.2,"}})}"
	         
	         ,"$$")),
	
	p("Your entries resulted in the following values:"),
	
	uiOutput("Mod4Step3_Phenotopic_correlation"),
	
	p("Note that if you have entered different values for the within-individual correlation 
		($r_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$) versus the among-individual correlation 
		($r_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}$), the overall phenotypic correlation is not 
		accurately reflecting either one. Only when you would have entered the same value and the same sign for both 
		the within- and among-individual correlation, would the overall phenotypic correlation be the same. 
		So do try different values for the two types of correlations and check how this affects the overall phenotypic 
		correlation in your data."),
	
	p("Another issue that you may investigate by changing the entry values above is the role of repeatability. 
		You may for example choose within- and among-individual correlations of equal strength but opposite sign 
		(e.g. -0.8 vs. 0.8) for a range of different repeatabilities. You could also play with creating 
		a dataset where one trait has a high and another trait has a low repeatability. 
		You will find that when both traits are highly repeatable, the phenotypic correlation will more 
		closely reflect the among-individual correlation. By contrast, if both traits have a low repeatability, 
		the phenotypic correlation will more closely reflect the within-individual correlations."),
	
	p("But how can we visualize the patterns of correlation within vs. among individuals? 
		We can do this by producing three types of scatter plots."),

	p("First, we can simply plot the raw data in a scatter plot; 
		this plot represents the overall phenotypic association between the two traits:"),

	# Simulation run button
	actionButton("Mod4Step3_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
	runningIndicator(),
	sim_msg(),
	
	plotOutput("Mod4Step3_correlationplot", width = Modules_VAR$Plot$width),
	
	p("Second, we can calculate each individual's mean value for each of the two traits, 
		and plot these two values in a scatter plot. This plot of individual-mean values represents 
		a visual of the among-individual correlation:"),
	
	p(),
	plotOutput("Mod4Step3_correlationplot2", width = Modules_VAR$Plot$width),
	
	p("Third, we can calculate how each observation deviates from an individual's mean value for 
		each of the two traits, and plot these two values in a scatter plot. This plot of 
		within-individual deviations from individual-means represents a visual of the within-individual correlation:"),
	
	p(),
	plotOutput("Mod4Step3_correlationplot3", width = Modules_VAR$Plot$width),
	
	p(HTML("<b>Conclusion:</b> Repeatedly expressed traits often vary across multiple hierarchical levels, 
		such as within and among individuals. This means that the phenotypic correlation summarizes 
		the correlations existing at different levels but that their respective influences are weighted 
		by the amount of variance existing at each level. In other words, the correlation between 
		two traits that have low repeatabilities (e.g. physiology, behaviour) will largely represent 
		within-individual correlations, whereas correlations between traits that have high repeatabilities 
		(e.g. morphology) will largely represent among-individual correlations. For this reason it is 
		important to partition the phenotypic correlation into its underlying components when one is 
		interested in level-specific patterns of correlations, such as correlations within individuals 
		due to trade-offs.")),
	
	
	p(strong("References:")),
	p(HTML("van Noordwijk, A.J. & de Jong, G. (1986) Acquisition and allocation of resources - 
	       Their influence on variation in life-history tactics. <i>American Naturalist</i>, 128, 137-142.
	       <a href='https://doi.org/10.1086/284547' target='_blank'>doi: 10.1086/284547</a>")),
	
	div(class = "line"),
	
	actionLink("Mod4Step3GotoStep2", label = "<< Previous Step (2)", class = "linkToModuleSteps"), # Go to previous step
	span(Modules_VAR$StepLink$sep, class = "step-Link"),
	actionLink("Mod4Step3GotoStep4", label = "Next Step (4) >>", class = "linkToModuleSteps") # Go to next step

)