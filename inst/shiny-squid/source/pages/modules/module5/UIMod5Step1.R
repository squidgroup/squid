# UI: Module 5 Step 1
span( 
  
	
	p(HTML(paste0("In the <i>",Module_titles$mod3,"</i> module, we accounted for the effect of a single environmental 
		factor on phenotype. This statistical process ends up describing the organism's phenotype 
		as a line in uni-dimensional environment (the environment on the X-axis, phenotype on the Y-axis). 
		For example, parent birds typically increase the rate of food delivery to the nest as their offspring grow older. 
		We call this line a reaction norm, and in the case of parent birds, it makes adaptive sense because older 
		offspring are bigger and need a faster food intake to maintain growth."))),
	
	p('A simple reaction norm line in a uni-dimensional environment is a simplified scenario, 
		because we know organisms exist in environments that have multiple environmental factors 
		(e.g., they are multidimensional). There is increasing evidence that multiple factors 
		can influence many phenotypes. In the case of parent birds increasing food delivery to
		older offspring, maintaining the rate of increase with age over all environmental conditions 
		makes little sense. If, for example, brood size varies among nesting attempts,
		we might expect parents to respond to both nestling age and brood size. 
		For parent birds, the environment now has two dimensions, nestling age and brood size, 
		and instead of a reaction norm line, their behavioural response could be described by a 
		plane in this 2 dimensional space. This is what we have called "multi-dimensional phenotypic plasticity" 
		or MDPP, and in theory an organism could be responding to many environmental factors, 
		so they would have a reaction norm plane existing in n-dimensional environmental space. 
		It is hard for us to visualize past three dimensions, so here we will focus on the behaviour 
		of a reaction norm plane in just two environmental axes, but if you get comfortable with this, 
		it will be easier to imagine three or more environmental axes. Our general goal is to help you 
		gain some understanding of how specific parameter values in an analysis equation influence 
		shape and orientation of a reaction norm plane.'),
	
	
	# Text: title
  h4("Step 1: Population level MDPP"),

  # Sub-goal
  p(HTML("<b>Sub-goal:</b> To understand average (population-level) effects of multiple environmental factors on the phenotype.")),

  # conditionalPanel(
  #   condition = "0",
  #   uiOutput("Mod5Step1_hidden")
  # ),

  # Introduction
  p(HTML(paste0("<b>Introduction:</b> Here we model multiple sources of environmental variance ($V_",NOT$envEffect,"$) in a single trait, 
  							expressed multiple times within individuals but measured within a single population. 
  							This step illustrates some differences between simple and multiple regression, but may 
  							also allow simulation of more complex data structures, such as correlations between environments (see step 4)."))),

  # Exercise
  p(HTML("<b>Exercise:</b> to explore multiple sources of $V_",NOT$envEffect,"$.")),


	# Input: Number of individuals
	strong("---------> Input: Number of individuals"),
	
	# Input: Among-individual variance (Vi)
	strong("---------> Input: Among-individual variance (Vi)"),
	
	# Input: NMeasurement error variance
	strong("---------> Input: Measurement error variance"),
	
	# Input: Number of trait expression measured
	strong("---------> Input: Number of trait expression measured"),
	
	
	p("Note: For now, we will assume all individuals are sampled equally often and at the same time."),
	
	strong("The environment"),
	
	p(HTML(paste0("Let’s simulate phenotypes that are influenced by two factors, both of which are shared by the 
		whole population (e.g., spring temperature) and the values are random from one measurement period 
		to the next. The environment thus has an intercept effect on phenotype of 0, and a slope that 
		you can input (we recommend at first that the slope be >> 0). Each environment contributes 
		the value $[slope]^2Var(",NOT$env,")$ to the total phenotypic variance (see Table 1 in Allegue <i>et al.</i> 2016), 
		so by specifying the slope (positive or negative), you will affect the total phenotypic variance. 
		Note that in SQuID each environmental effect is standardized (i.e. mean = 0 and variance = 1)."))),
	
	p("Enter the slope for each environmental factor. These can be either positive or negative. "),
	
	# Input: Environment 1
	strong("---------> Input: Environment 1"),
	
	# Input: Environment 2
	strong("---------> Input: Environment 2"),
	
	#   p(),
	#   # Simulation run button
	#   actionButton("Mod5Step1_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
	#   runningIndicator(),
	#   p(),
	
	strong("Results"),
	
	p("Suppose we assume that there is only one environmental effect. That is, we analyse 
		the population we simulated using the following model:"),
	
	# Equation
	p(paste0("$$",
							NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
							
							",EQ1$mean0," +
							",EQ1$dev1,"  +
							",EQ1$mean1,NOT$env,"_{1",NOT$time,NOT$ind,"} +
						  ",NOT$error,"_{",NOT$time,NOT$ind,"$$")),
	
	p("A mixed statistical model estimates the parameters:"),
	
	p("Statistical output:"),
	
	# Output: Table 1
	strong("---------> Output: Table 1"),
	
	p(HTML(paste0("This makes the simple point, also made in Module <i>",Module_titles$mod3,"</i>, 
		that leaving out an important factor inflates other variance components. In this case 
		it was mostly the residual variance because the environment was set as random from one 
		measurement to the next and all individuals experienced it."))),
	
	p("A reanalysis with the following model pulls the missing environmental variance out of the residual term:"),
	
	# Equation
	p(paste0("$$",
					 NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
					 
					 ",EQ1$mean0," +
					 ",EQ1$dev1,"  +

					 ",EQ1$mean1,NOT$env,"_{1",NOT$time,NOT$ind,"} +
					 ",EQ1$mean2,NOT$env,"_{2",NOT$time,NOT$ind,"} +

					 ",NOT$error,"_{",NOT$time,NOT$ind,"$$")),
	
	p("Statistical output:"),
	
	# Output: Table 2
	strong("---------> Output: Table 2"),
	
	p("This is a multiple regression within a mixed model. A 3-dimensional graph helps visualize 
		the way in which the two x variables affect a phenotype in the 2 dimensions defined by the environment."),
	
	p("This is a multiple regression within a mixed model. A 3-dimensional graph helps visualize the way in which the 
		two x variables affect a phenotype in the 2 dimensions defined by the environment."),
	
	# Output: Figure 1
	strong("---------> Output: Figure 1"),
	
	p(paste0("Individuals in this simulation vary in their intercept by the amount you entered (....). 
		Below we pick three individuals across the range of the intercept variance to illustrate 
		how each individual's plane sits in the space defined by the two environmental variables. 
		You can see that the three planes are parallel or very close to parallel, and differ only 
		in their elevation. If you play around with the number of measures within an individual, 
		you will see that the resolution of these planes requires fairly large sample sizes 
		(this is covered in more detail later).")),
	
	p(paste0("New 3-D graph with three individuals picked from the low end of $",NOT$devI,"$, 
					 the middle, and from the high end of $",NOT$devI,"$.")),
	
	p(paste0("Run through this simulation several times using different values for ",EQ1$mean1," and ",EQ1$mean2,", 
					 including having some slopes negative. In particular, try making the two have opposite signs.
					 Inspect the table above and look at the two graphs so you gain a feel for how the two slopes 
					 produce a flat plane that may be tilted in various ways.")),
	
  p(HTML("<b>Conclusion:</b> is exercise should reinforce your understanding of where measured 
  			 and unmeasured sources of variance end up in a statistical analysis and how systematic 
  			 effects of multiple environments can be appropriately captured. In the next step, 
  			 we illustrate one important complexity.")),

    div(class = "line"),

    # Go to next step
    actionLink("Mod5Step1GotoStep2",
             label = "Next Step (2) >>",
             class = "linkToModuleSteps")
)