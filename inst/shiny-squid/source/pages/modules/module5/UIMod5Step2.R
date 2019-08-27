# UI: Module 5 Step 2
span( 

	# Text: title
  h4("Step 2: Interaction terms"),

  # Sub-goal
  p(HTML("<b>Sub-goal:</b> To account for dependencies in the effect of one factor by another factor.")),

  conditionalPanel(
    condition = "0",
    uiOutput("Mod5Step2_hidden")
  ),

  # Introduction
  p(HTML("<b>Introduction:</b> In step 1 above, we developed the idea that 2 or more environments 
					might both have effects on a phenotype. In the case of parent birds feeding offspring, 
					both offspring age and the number of offspring were hypothesized to have effects on a parent's 
					feeding rate. The analysis in Step 1 produced a flat plane in 2-dimensional environmental 
					space that then could possibly be tilted in a variety of ways depending on the slopes to 
					both environments. The effect of brood size and nestling age is an intriguing case because 
					the increase in need as a nestling gets older is not merely added to the effect of brood size, 
					but rather brood size multiplies the effect of nestling age (the effect of age for 4 nestlings is 4 
					times the effect for 1 nestling). More generally, we might expect plasticity to one environmental 
					variable to be plastic in the face of other environmental variables. This plasticity in response 
					to one environment of a reaction norm to another (plasticity of plasticity) is a fascinating 
					potential consequence of multidimensional environments.")),
  
  p(HTML("So far, you have explored the effect of two environmental factors that produce a flat plane. 
  	The second statistical equation in step 1 illustrates these as additive effects of the two environmental 
  	factors. Put another way, the effects of the factors were treated as independent. Note the distinction 
  	between independence of effects and independence of the factors themselves&mdash;weak to modest correlations 
  	among the factors themselves do not change the independence of the effects of those factors, as will 
  	be illustrated in a later module. However, high correlations between factors (colinearity) can have 
  	unusual effects on statistical tests of parameter estimates&mdash;we ignore that issue for now. Instead, 
  	plasticity of plasticity as described above results from non-additive effects. The factors are said 
  	to interact, the term describing them in a mixed model or any related analysis (GLM) is an 
  	interaction term. Biologically, interactive effects on a phenotype between 2 environmental factors 
  	may have multiple fascinating implications.")),
  
  # Exercise
  p(HTML("<b>Exercise:</b> As in step 1, we need to simulate a population and the data we collect from that population.")),

  # Input: Number of individuals
  getSliderInput("Mod5Step2_NI", Modules_VAR$NI),
  
  # Input: Among-individual variance (Vi)
  getSliderInput("Mod5Step2_Vi", Modules_VAR$Vi),
  
  # Input: NMeasurement error variance
  getSliderInput("Mod5Step2_Ve", Modules_VAR$Vm),
  
  # Input: Number of trait expression measured
  getSliderInput("Mod5Step2_NR", Modules_VAR$NR),

  info_msg("For now, we will assume all individuals are sampled equally often and at the same time."),

	p(strong("The environment")),

	p("Enter the slope for each environmental factor and the interaction term. These can be either positive or negative."),

  # Input: Environment 1
  getSliderInput("Mod5Step2_B1", Modules_VAR$B1.1),
  
  # Input: Environment 2
  getSliderInput("Mod5Step2_B2", Modules_VAR$B2.1),
  
  # Input: Interaction
  getSliderInput("Mod5Step2_B12", Modules_VAR$B1122),
  
  # Simulation run button
  actionButton("Mod5Step2_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
  runningIndicator(),
  sim_msg(),

	p(strong("Results")),

  p("Let's analyze this simulated population by omitting the interaction term first. 
  	In the first case we assume the following statistical model:"),
  
	# Equation 1
	p(paste0("$$",
							NOT$trait.1,"_{",NOT$time,NOT$ind,"} =

							",EQ1$mean0," +
							",EQ1$dev0,"  +
							",EQ1$mean1,NOT$env,"_{1",NOT$time,NOT$ind,"} +
							",EQ1$mean2,NOT$env,"_{2",NOT$time,NOT$ind,"} +

						  ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  displayRCode("# install.packages(&quot;lme4&quot;)<br>
                 LMM <- lme4::lmer(Phenotype ~ 1 + X1 + X2 + (1|Individual), data = sampled_data)"),
  
  p("The full statistical model including the interaction is:"),
  
  # Equation 2
  p(paste0("$$",
  				 NOT$trait.1,"_{",NOT$time,NOT$ind,"} =
  				 
  				 ",EQ1$mean0," +
  				 ",EQ1$dev0,"  +
  				 ",EQ1$mean1,NOT$env,"_{1",NOT$time,NOT$ind,"} +
  				 ",EQ1$mean2,NOT$env,"_{2",NOT$time,NOT$ind,"} +
     			 ",EQ1$mean12,NOT$env,"_{1",NOT$time,NOT$ind,"}",NOT$env,"_{2",NOT$time,NOT$ind,"} +
  				 
  				 ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  displayRCode("# install.packages(&quot;lme4&quot;)<br>
                 LMM <- lme4::lmer(Phenotype ~ 1 + X1 + X2 + X1X2 + (1|Individual), data = sampled_data)"),
  
  p("Here the results compared:"),
  
	p("Statistical output:"),

  # Output: Table 1
  uiOutput("Mod5Step2_summary_table"),
  
  p("You should find from the above that the variance caused by the interaction term, when that term is omitted,
  	ends up mostly in the residuals, although some may end up elsewhere due to sampling issues."),

  p("You can visualize the impact of the interaction term in the graph below. Here we have graphed
  	the population average plane derived from the parameter estimates in the simulated data in the space defined by both environments."),

  # Output: Figure 1
  # p(uiOutput("Mod5Step2_3D")),
  info_msg("The grey plane in the figures represents the population mean phenotypic plane."),
  p(plotlyOutput("Mod5Step2_3D")),
  
  p(paste0("Examine this graph carefully. The plane produced should look different than the ones you
  				 produced in step 1. Those planes were flat but tilted in various ways. If $",EQ1$mean12,"$ is not 0,
  				 the plane in this graph should look warped or bent. This is the influence of the interaction term.")),

  p(paste0("Now redo the above and manipulate both the magnitude of the interaction between $",NOT$env,"_1$ and $",NOT$env,"_2$
  				 ($",EQ1$mean12,"$) and its direction relative to the other slopes to assess how this affects your
  				 results if you leave it out of your statistical analysis.")),

  p(paste("You can also see in the graph how the parameter $",EQ1$mean12,"$ changes the warping of the plane.")),

  p(HTML("<b>Conclusion:</b> Multidimensionality of environmental effects on phenotypic attributes is very likely.
  			 It may seem redundant to keep demonstrating that leaving out an important term causes that variance to
  			 end up in other terms. The unusual element of interactions is that the direction of the slope compared
  			 to the main effects matters also. Moreover, interaction terms generate an array of interesting biological
  			 questions about both the way organisms integrate information about environment and the selective forces
  			 shaping the reaction norm plane.")),

  div(class = "line"),

  actionLink("Mod5Step2GotoStep1", label = "<< Previous Step (1)", class = "linkToModuleSteps") # Go to previous step
)