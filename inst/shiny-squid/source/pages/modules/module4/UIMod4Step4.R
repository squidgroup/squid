# UI: Module 4 Step 4
span( 
  
	# Text: title
	h4("Step 4: The Bivariate mixed-effects model."),
	
	# Sub-goal
	p(HTML("<b>Sub-goal:</b> Understanding the difference between a univariate and a multivariate mixed-effects model, 
				 and how level-specific correlations are modelled.")), 
	
	
	# conditionalPanel(
	# 	condition = "0",
	# 	uiOutput("Mod4Step4_hidden")
	# ),
	
	# Introduction
	p(HTML(paste0("<b>Introduction:</b> In the previous modules (",Module_titles$mod1,", ",Module_titles$mod3,", and ",
								Module_titles$mod6,") we have considered univariate mixed-effects models. As you have seen, 
								the univariate mixed-effects model enabled us to estimate the variance attributable to variation within- 
								and among-individuals in a single trait (",NOT$trait.1,") with the following equation:"))),
	
	p(paste0("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
							(",EQ3$mean0," + ",NOT$devI,"_",NOT$ind,") + 
					     ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
	
	p(HTML(paste0("The variation in intercepts ($V_",NOT$devI,"$) among individuals was assumed to be normally distributed (N) 
					 with a mean of zero and a variance ($\\Omega_{",NOT$devI,"}$) and is called the <i>among-individual variance</i> 
					 (estimated as $V_",NOT$devI,"$: the variance across random intercepts of individuals):"))),
	
	p(paste0("$$[",NOT$devI,"_",NOT$ind,"] \\sim N(0, \\Omega_{",NOT$devI,"}): \\Omega_{",NOT$devI,"} = [V_",NOT$devI,"]$$")),
	
	p(HTML(paste0("A residual error ($",NOT$error,"_{",NOT$time,NOT$ind,"}$) was also assumed to be normally distributed, with zero mean 
								and a variance ($\\Omega_{",NOT$error,"}$) representing the <i>within-individual variance</i>:"))),
	
	p(paste0("$$[",NOT$error,"_{",NOT$time,NOT$ind,"}] \\sim N(0, \\Omega_{",NOT$error,"}): \\Omega_{",NOT$error,"} = [V_",NOT$error,"]$$")),
	
	p(HTML(paste0("In the bivariate mixed-effects models, we are estimating these parameters simultaneously for two traits. 
								That is, the model can be formulated as a set of two phenotypic equations (one for ",NOT$trait.1," and one for ",NOT$trait.2,"):"))),

	p(paste0("$$",
					 NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
							(",EQ$mean0.1," + ",EQ$dev0.1,") + 
					 ",NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,"}$$

					$$", 
					 NOT$trait.2,"_{",NOT$time,NOT$ind,"} = 
					 	(",EQ$mean0.2," + ",EQ$dev0.2,") + 
					 	",NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,"}
					 $$")),
	
	p(paste0("As was the case for univariate models, the random intercepts ($",NOT$devI,"_",NOT$ind,"$) 
					 and the within-individual contributions ($",NOT$error,"_{",NOT$time,NOT$ind,"}$) to ",NOT$trait.1," and ",NOT$trait.2," are modelled as
					 having means of zero. However, in this bivariate case, neither the random 
					 intercepts nor the residual errors are independent. Instead, the random intercepts 
					 are now distributed assuming a multivariate normal distribution with a variance-covariance structure 
					 ($\\Omega_{",NOT$devI,"}$) specifying the among-individual variances ($V_{",NOT$devI,"_",NOT$trait.1,"}$ and $V_{",NOT$devI,"_",NOT$trait.2,"}$) 
					 and the among-individual covariance between the two attributes ($Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}$): ")),
	
	
	p(paste0(
		"$$ \\Omega_{",NOT$devI,"}=
		\\begin{pmatrix}
		Var(",NOT$devI,"_",NOT$trait.1,") & Cov(",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,") \\\\
		Cov(",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,") & Var(", NOT$devI,"_",NOT$trait.2,")\\\\
		\\end{pmatrix} 
		$$")),
	
	
	
	# p("Set the amount of within-individual variance in each of the two traits:"),
	# # Within-individual variances (Ve)
	# fluidRow(
	# 	column(6,getSliderInput("Mod4Step3_Ve1", Modules_VAR$Ve1)),
	# 	column(6,getSliderInput("Mod4Step3_Ve2", Modules_VAR$Ve2))
	# ),
	# 
	# p("Set the within-individual correlation, which should be negative in case female 
	# 	trade-off their investment in egg size versus number"),
	# # Within-individual correlation
	# getSliderInput("Mod4Step3_Corr_e", Modules_VAR$Corr_e),
	# 
	# p("Set the amount of among-individual variance in each of the two traits:"),
	# fluidRow(
	# 	column(6,getSliderInput("Mod4Step3_Vi1", Modules_VAR$Vi1)),
	# 	column(6,getSliderInput("Mod4Step3_Vi2", Modules_VAR$Vi2))
	# ),
	# 
	# p("Set the among-individual correlation, which should be positive if females with more resources can 
	# 	produce both large eggs and large clutches."),
	# # Among-individual correlation
	# getSliderInput("Mod4Step3_Corr_I", Modules_VAR$Corr_I),
	# 
	# p(strong("Results:")),
	# 
	# p('You have defined the elements of the within-individual "variance-covariance matrix":'),
	# p(paste0(
	# 	"$$ \\Omega_{",NOT$error,"}=
	# 	\\begin{pmatrix}
	# 	Var(",NOT$error,"_",NOT$trait.1,") & Cov(",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,") \\\\
	# 	Cov(",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,") & Var(", NOT$error,"_",NOT$trait.2,")\\\\
	# 	\\end{pmatrix} 
	# 	$$")),
	# 
	# p("Your entries resulted in the following values for this matrix:"),
	# 
	# ######## Matrix with the values entered
	# uiOutput("Mod4Step3_Within_Covariance_Matrix"),
	# 
	# p("You have now also defined the elements of the among-individual matrix:"),
	# p(paste0(
	# 	"$$ \\Omega_{",NOT$devI,"}=
	# 	\\begin{pmatrix}
	# 	Var(",NOT$devI,"_",NOT$trait.1,") & Cov(",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,") \\\\
	# 	Cov(",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,") & Var(", NOT$devI,"_",NOT$trait.2,")\\\\
	# 	\\end{pmatrix} 
	# 	$$")),
	# 
	# p("Your entries resulted in the following values for this matrix:"),
	# 
	# ######## Matrix with the values entered
	# uiOutput("Mod4Step3_Among_Covariance_Matrix"),
	# 
	# p("As you have set both the among- and within-individual variances, 
	# 	you have set the repeatabilities of both traits. Those are:"),
	# 
	# ###### Repeatabilities OUTPUT  
	# uiOutput("Mod4Step3_Repeatabilities"),
	# 
	# p("We will now again return to the equation describing the components affecting the phenotypic correlation:"),
	# 
	# ####### eq 1
	# p(paste0("$$",
	#          "r_{",NOT$total,"_",NOT$trait.1,",",NOT$total,"_",NOT$trait.2,"} = 
	#          
	#          r_{",NOT$devI,"_" ,NOT$trait.1,",",NOT$devI,"_" ,NOT$trait.2,"}
	#          \\sqrt{
	#          (\\frac{V_{",NOT$devI,"_" ,NOT$trait.1,"}}
	#          {V_{",NOT$devI,"_" ,NOT$trait.1,"} + V_{",NOT$error,"_" ,NOT$trait.1,"}})
	#          (\\frac{V_{",NOT$devI,"_" ,NOT$trait.2,"}}
	#          {V_{",NOT$devI,"_" ,NOT$trait.2,"} + V_{",NOT$error,"_" ,NOT$trait.2,"}})} + 
	#          
	#          r_{",NOT$error,"_" ,NOT$trait.1,",",NOT$error,"_" ,NOT$trait.2,"}
	#          \\sqrt{
	#          (\\frac{V_{",NOT$error,"_" ,NOT$trait.1,"}}
	#          {V_{",NOT$devI,"_" ,NOT$trait.1,"} + V_{",NOT$error,"_" ,NOT$trait.1,"}})
	#          (\\frac{V_{",NOT$error,"_" ,NOT$trait.2,"}}
	#          {V_{",NOT$devI,"_" ,NOT$trait.2,"} + V_{",NOT$error,"_" ,NOT$trait.2,"}})}"
	#          
	#          ,"$$")),
	# 
	# p("Your entries resulted in the following values:"),
	# 
	# uiOutput("Mod4Step3_Phenotopic_correlation"),
	# 
	# p("Note that if you have entered different values for the within-individual correlation 
	# 	($r_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$) versus the among-individual correlation 
	# 	($r_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}$), the overall phenotypic correlation is not 
	# 	accurately reflecting either one. Only when you would have entered the same value and the same sign for both 
	# 	the within- and among-individual correlation, would the overall phenotypic correlation be the same. 
	# 	So do try different values for the two types of correlations and check how this affects the overall phenotypic 
	# 	correlation in your data."),
	# 
	# p("Another issue that you may investigate by changing the entry values above is the role of repeatability. 
	# 	You may for example choose within- and among-individual correlations of equal strength but opposite sign 
	# 	(e.g. -0.8 vs. 0.8) for a range of different repeatabilities. (You could also play with creating 
	# 	a dataset where one trait has a high and another trait has a low repeatability). 
	# 	You will find that when both traits are highly repeatable, the phenotypic correlation will more 
	# 	closely reflect the among-individual correlation. By contrast, if both traits have a low repeatability, 
	# 	the phenotypic correlation will more closely reflect the within-individual correlations."),
	# 
	# p("But how can we visualize the patterns of correlation within vs. among individuals? 
	# 	We can do this by producing three types of scatter plots."),
	# 
	# p("First, we can simply plot the raw data in a scatter plot; 
	# 	this plot represents the overall phenotypic association between the two traits:"),
	# 
	# p(),
	# # Simulation run button
	# actionButton("Mod4Step3_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
	# runningIndicator(),
	# p(),
	# 
	# p(),
	# plotOutput("Mod4Step3_correlationplot", width = Modules_VAR$Plot$width),
	# 
	# p("Second, we can calculate each individual's mean value for each of the two traits, 
	# 	and plot these two values in a scatter plot. This plot of individual-mean values represents 
	# 	a visual of the among-individual correlation:"),
	# 
	# p(),
	# plotOutput("Mod4Step3_correlationplot2", width = Modules_VAR$Plot$width),
	# 
	# p("Third, we can calculate how each observation deviates from an individual's mean value for 
	# 	each of the two traits, and plot these two values in a scatter plot. This plot of 
	# 	within-individual deviations from individual-means represents a visual of the within-individual correlation:"),
	# 
	# p(),
	# plotOutput("Mod4Step3_correlationplot3", width = Modules_VAR$Plot$width),
	# 
	# p(HTML("<b>Conclusion:</b> Repeatedly expressed traits often vary across multiple hierarchical levels, 
	# 	such as within and among individuals. This means that the phenotypic correlation summarizes 
	# 	the correlations existing at different levels but that their respective influences are weighted 
	# 	by the amount of variance existing at each level. In other words, the correlation between 
	# 	two traits that have low repeatabilities (e.g. physiology, behaviour) will largely represent 
	# 	within-individual correlations, whereas correlations between traits that have high repeatabilities 
	# 	(e.g. morphology) will largely represent among-individual correlations. For this reason it is 
	# 	important to partition the phenotypic correlation into its underlying components when one is 
	# 	interested in level-specific patterns of correlations, such as correlations within individuals 
	# 	due to trade-offs.")),
	# 
	# 
	# p(strong("References:")),
	# p(HTML("van Noordwijk, A.J. & de Jong, G. (1986) Acquisition and allocation of resources - 
	#        Their influence on variation in life-history tactics. <i>American Naturalist</i>, 128, 137-142.")),
	
	div(class = "line"),
	
	actionLink("Mod4Step3GotoStep2", label = "<< Previous Step (2)", class = "linkToModuleSteps"), # Go to previous step
	span(Modules_VAR$StepLink$sep, class = "step-Link"),
	actionLink("Mod4Step3GotoStep4", label = "Next Step (4) >>", class = "linkToModuleSteps") # Go to next step

)