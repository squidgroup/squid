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
								That is, the model can be formulated as a set of two phenotypic equations (one for $",NOT$trait.1,"$ and one for $",NOT$trait.2,"$):"))),

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
		V_{",NOT$devI,"_",NOT$trait.1,"} & Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"} \\\\
		Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"} & V_{", NOT$devI,"_",NOT$trait.2,"}\\\\
		\\end{pmatrix} 
		$$")),
	
	p("The residual errors ($",NOT$error,"_{",NOT$time,NOT$ind,"}$) are likewise assumed to be drawn from a multivariate normal distribution, 
		with means of zero, within-individual variances ($V_{",NOT$error,"_",NOT$trait.1,"}$ and $V_{",NOT$error,"_",NOT$trait.2,"}$), and within-individual 
		covariances ($Cov_{",NOT$error,"_{",NOT$trait.1,"},",NOT$error,"_{",NOT$trait.2,"}}$):"),
	
	p(paste0(
		"$$ \\Omega_{",NOT$error,"}=
		\\begin{pmatrix}
		V_{",NOT$error,"_",NOT$trait.1,"} & Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}  \\\\
		Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"} & V_{", NOT$error,"_",NOT$trait.2,"} \\\\
		\\end{pmatrix} 
		$$")),
	
	p("From these estimated matrices, we can calculate the phenotypic variances for 
		each trait by adding up the variances estimated at each level:"),
	
	p(paste0("$$V_{",NOT$total,"_",NOT$trait.1,"} = V_{",NOT$devI,"_",NOT$trait.1,"} + V_{",NOT$error,"_",NOT$trait.1,"}$$
					  $$V_{",NOT$total,"_",NOT$trait.2,"} = V_{",NOT$devI,"_",NOT$trait.2,"} + V_{",NOT$error,"_",NOT$trait.2,"}$$")),
	
	p("In the same fashion, we can calculate the phenotypic covariance between the 
		two traits by adding up the covariances estimated at each level:"),
	
	p(paste0("$$Cov_{",NOT$total,"_",NOT$trait.1,", ",NOT$total,"_",NOT$trait.2,"} = 
					 Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"} + 
					 Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$$")),
	
	p("With this information in hand, we can now calculate the overall phenotypic correlation in the data."),
	
	p(paste0("$$r_{",NOT$total,"_",NOT$trait.1,", ",NOT$total,"_",NOT$trait.2,"} = 
					 \\frac{Cov_{",NOT$total,"_",NOT$trait.1,",",NOT$total,"_",NOT$trait.2,"}}
					 {\\sqrt{V_{",NOT$total,"_",NOT$trait.1,"}V_{",NOT$total,"_",NOT$trait.2,"}}}$$")),
	
	strong("Conclusion: "),
	p("Bivariate mixed-effects models differ distinctly from univariate mixed-effects 
		models as the former assumes multivariate normality while the latter assumes univariate normality. 
		Bivariate mixed-effects models estimate variances and covariances within and among each specified 
		level from which overall phenotypic variances and covariances, as well as correlation, 
		can be subsequently derived."),
	
	div(class = "line"),
	
	actionLink("Mod4Step4GotoStep3", label = "<< Previous Step (3)", class = "linkToModuleSteps"), # Go to previous step
	span(Modules_VAR$StepLink$sep, class = "step-Link"),
	actionLink("Mod4Step4GotoStep5", label = "Next Step (5) >>", class = "linkToModuleSteps") # Go to next step

)