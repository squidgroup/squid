# UI: Module 4 Step 3
span( 
  
	# Text: title
	h4("Step 5: Environmentally-induced correlations."),
	
	# Sub-goal
	p(HTML("<b>Sub-goal:</b> Introducing integration of plasticity as a mechanism causing correlations.")), 
	

	conditionalPanel(
		condition = "0",
		uiOutput("Mod4Step5_hidden")
	),

	# Introduction
	p(HTML(paste0("<b>Introduction:</b> In the previous steps we have, considered a bivariate phenotypic 
								equation where two traits correlated among- and/or within-individuals without considering 
								plasticity as a mechanism causing this variation. This is why the double equation 
								that we re-print here harboured no fixed effects apart from the statistical intercepts:"))),

	p(paste0("$$",
					 NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.1," + ",EQ$dev0.1,") + 
					 ",NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,"}$$
					 
					 $$", 
					 NOT$trait.2,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.2," + ",EQ$dev0.2,") + 
					 ",NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,"}
					 $$")),
	
	p(paste0('In this final step, we will investigate the role of within-individual plasticity in generating correlations 
					 among repeatedly expressed traits. Returning to our example of egg size and egg number, 
					 we can imagine that individuals breed repeatedly across a set of years, sometimes encountering "good" 
					 and sometimes encountering "bad" years. We might expect females to plastically adjust both their eggs 
					 size and their clutch size to food availability, where we expect females to down-regulate both traits 
					 when encountering reduced food availability. We can model this situation by including food availability 
					 ($',NOT$env,'_{',NOT$time,NOT$ind,'}$) as a fixed-effect covariate affecting both egg size 
					 ($',EQ$mean1.1,NOT$env,'_{',NOT$time,NOT$ind,'}$) and egg number ($',EQ$mean1.2,NOT$env,'_{',NOT$time,NOT$ind,'}$):')),
	
	p(paste0("$$",
					 NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.1," + ",EQ$dev0.1,") + 
					  ",EQ$mean1.1,NOT$env,"_{",NOT$time,NOT$ind,"} +
					 ",NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,"}$$
					 
					 $$", 
					 NOT$trait.2,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.2," + ",EQ$dev0.2,") + 
 					 ",EQ$mean1.2,NOT$env,"_{",NOT$time,NOT$ind,"} +
					 ",NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,"}
					 $$")),

	
	p("To keep matters simple, we will assume in this example that the two traits are neither correlated within 
		nor correlated among individuals due to other mechanisms than variation in food availability. 
		At the same time, we are assuming that both traits harbour both among- and within-individual variation. "),
	
	# Exercise
	p(HTML("<b>Exercise:</b> We will generate a dataset for 100 individuals each sampled 10 times . 
				 Set the within- and among-individual variances for the two traits. We suggest that you create a dataset 
				 where the associated repeatabilities are 0.5. You will also have to set the within- and among-individual correlations. 
				 For the purpose of this example, we suggest that you set their values to zero. We will assume that the environment 
				 is changing stochastically between the 10 time steps, and that the average phenotype in the population has a value of zero.")),

	p("Set the amount of within-individual variance in each of the two traits:"),
	# Within-individual variances (Ve)
	fluidRow(
		column(6,getSliderInput("Mod4Step5_Ve1", Modules_VAR$Ve1)),
		column(6,getSliderInput("Mod4Step5_Ve2", Modules_VAR$Ve2))
	),
	
	p("Set the within-individual correlation:"),
	# Within-individual correlation
	getSliderInput("Mod4Step5_Corr_e", Modules_VAR$Corr_e),
	
	p("Set the amount of among-individual variance in each of the two traits:"),
	fluidRow(
		column(6,getSliderInput("Mod4Step5_Vi1", Modules_VAR$Vi1)),
		column(6,getSliderInput("Mod4Step5_Vi2", Modules_VAR$Vi2))
	),
	
	p("Set the among-individual correlation:"),
	# Among-individual correlation
	getSliderInput("Mod4Step5_Corr_I", Modules_VAR$Corr_I),
	
	p("Set population-level slope (i.e., level of plasticity to food availability) for each trait:"),
	fluidRow(
		column(6,getSliderInput("Mod4Step5_B11", Modules_VAR$B11)),
		column(6,getSliderInput("Mod4Step5_B12", Modules_VAR$B12))
	),
	
	p("You have set the following bivariate phenotypic equation:"),
	
	p(paste0("$$",
					 NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.1," + ",EQ$dev0.1,") + 
					 ",EQ$mean1.1,NOT$env,"_{",NOT$time,NOT$ind,"} +
					 ",NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,"}$$
					 
					 $$", 
					 NOT$trait.2,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.2," + ",EQ$dev0.2,") + 
					 ",EQ$mean1.2,NOT$env,"_{",NOT$time,NOT$ind,"} +
					 ",NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,"}
					 $$")),
	
	p("Your entries resulted in the following values:"),
	uiOutput("Mod4Step5_Phenotypic_Equation"),
	
	p("You have also defined the elements of the among-individual and 
		residual within-individual variance-covariance matrices:"),
	
	uiOutput("Mod4Step5_Matrices_1"),
	
	
	p("As we have seen in previous steps, we can visualize the patterns at the 
		overall phenotypic level, within individuals, and among individuals."),
	
	p("First, we plot the raw data in a scatter plot; this plot represents 
		the overall phenotypic association between the two traits:"),
	
	# Simulation run button
	actionButton("Mod4Step5_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
	runningIndicator(),
	sim_msg(),
	
	plotOutput("Mod4Step5_correlationplot", width = Modules_VAR$Plot$width),
	
	
	p("Second, we calculated each individual's mean value for each of the two traits, 
		and plotted these two values in a scatter plot. This plot of individual-mean values 
		represents a visual of the among-individual correlation:"),
	
	p(),
	plotOutput("Mod4Step5_correlationplot2", width = Modules_VAR$Plot$width),
	
	
	p("Third, we calculated how each observation deviates from an individual's mean value for 
		each of the two traits, and plot these two values in a scatter plot. This plot of 
		within-individual deviations from individual-means represents a visual of the within-individual correlation:"),
	
	plotOutput("Mod4Step5_correlationplot3", width = Modules_VAR$Plot$width),
	
	
	p("Here is also the plot showing how the environment is changing over time:"),
	
	plotOutput("Mod4Step5_environment", width = Modules_VAR$Plot$width),
	
	
	p("If you chose a within- and among-individual correlation equal to zero and also set non-zero 
		slopes for x for both traits, you will see that the data show a within-individual correlation. 
		The correlation emerges at this level because we set the simulation to produce a situation 
		where all individuals were sampled equally often and at the same time 
		(i.e., there was no variation in the timing of sampling). This meant that the environment 
		(food availability) varied within but not among individuals, and therefore caused correlations 
		between egg size and number within but not among individuals either."),
	
	p("We will now run a bivariate mixed-effects model on these data, where we will fit the 
		following phenotypic equation with the following random effects structures:"),
	
	p(paste0("$$",
					 NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.1," + ",EQ$dev0.1,") + 
					 ",NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,"}$$
					 
					 $$", 
					 NOT$trait.2,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.2," + ",EQ$dev0.2,") + 
					 ",NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,"}
					 $$")),
	
	uiOutput("Mod4Step5_Matrices_2"),
	
	# Simulation run button
	actionButton("Mod4Step5_Run_1", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
	runningIndicator(),
	sim_msg(),
	
	p("Here we print the point estimates derived from a Bayesian-analysis 
	  of these data (true values are between brackets):"),
	
	##### RESULTS	#######################################
	
	uiOutput("Mod4Step5_Result_Matrices_Model1"),
	
	p("With these values, we can calculate the point estimates for the level-specific correlations:"),
	
	uiOutput("Mod4Step5_Result_Matrices_Model1_corr"),
	
	displayRCode("# install.packages(&quot;brms&quot;)<br>
	              # for more inforation on the 
	              <a href='https://paul-buerkner.github.io/brms/' target = '_blank'><i>brms</i> package</a>.<br>
               library(brms) <br>
	             BLMM1 <- brm(mvbind(Phenotype_1, Phenotype_2) ~ 1 + (1|p|Individual), data  = sampled_data)"),

	####################################################
	
	p("Importantly, the estimates printed above are from a model where the effect of food availability (x) 
		was not modelled. Given the structure of the data, where food availability affected both traits but only 
		varied within individuals, the residual within-individual correlation will 
		thus be nonzero even if you modelled it to be zero."),
	
	p("We will now run an updated bivariate mixed-effects model on these data, where we will fit 
		same model but now we do model the effect of food availability (x) on egg size and number:"),
	
	p(paste0("$$",
					 NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.1," + ",EQ$dev0.1,") + 
					 ",EQ$mean1.1,NOT$env,"_{",NOT$time,NOT$ind,"} +
					 ",NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,"}$$
					 
					 $$", 
					 NOT$trait.2,"_{",NOT$time,NOT$ind,"} = 
					 (",EQ$mean0.2," + ",EQ$dev0.2,") + 
					 ",EQ$mean1.2,NOT$env,"_{",NOT$time,NOT$ind,"} +
					 ",NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,"}
					 $$")),

	uiOutput("Mod4Step5_Matrices_3"),
	
	# Simulation run button
	actionButton("Mod4Step5_Run_2", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
	runningIndicator(),
	sim_msg(),
	
	p("Here we print the point estimates derived from a Bayesian-analysis 
	  of these data (true values are between brackets):"),
	
	##### RESULTS	#######################################
	
	uiOutput("Mod4Step5_Result_Matrices_Model2"),
	
	p("With these values, we can calculate the point estimates for the level-specific correlations:"),
	
	uiOutput("Mod4Step5_Result_Matrices_Model2_corr"),
	
	displayRCode("# BLMM2 <- brm(mvbind(Phenotype_1, Phenotype_2) ~ X1 + (1|p|Individual), data  = sampled_data)"),
	
	####################################################
	
	p(paste0("This new model should properly recover the effect of x on y and z. 
					 You will notice that the residual within-individual covariance ($Cov_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$) 
					 should now better resemble the estimate that you should have expected based on 
					 the within-individual correlation that you entered previously. This is because 
					 the within-individual covariance is now explained by the fixed effect covariate, 
					 and therefore food availability no longer contributes to the residual within-individual variances.")),
	
	strong("Conclusion:"),
	p("Integration of plasticity, where the same environmental factor affects multiple phenotypic traits, 
		leads to patterns of trait correlations in phenotypic data. Provided that such environmental factors 
		varied solely within individuals, environmental variation would cause residual within-individual correlations 
		if not accounted for. Though not further articulated here, we can imagine that such types of environmental 
		effects could also cause among-individual correlations in situations where the sampling design was such 
		that it resulted in among-individual variation in the timing of sampling. In such situations, among- 
		and within-individual correlations would occur in the presence of integration of plasticity, provided 
		that its effects were not modelled in the fixed-effects structure of the statistical model. 
		We will explore this issue and others in another module (Sampling and Bivariate Models)."),
	
	div(class = "line"),
	
	actionLink("Mod4Step5GotoStep4", label = "<< Previous Step (4)", class = "linkToModuleSteps") # Go to previous step

)
