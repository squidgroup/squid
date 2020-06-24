# UI: Module 8 Step 1
span( 
  
	# Text: title
  h4("Step 1: Adding multiple slopes that vary among individuals"),

  # Sub-goal
  p(HTML("<b>Sub-goal:</b> Can we statistically test for individual variation in interaction 
         terms and what sampling regimes might improve the ability to do so?")),

  # conditionalPanel(
  #   condition = "0",
  #   uiOutput("Mod8Step1_hidden")
  # ),

  # Introduction
  p(HTML(paste0("<b>Introduction:</b> Multidimensional phenotypic plasticity is a widespread feature of most living organisms. 
                In many cases it appears to be adaptive. For example, copulating male dung flies stay in copula for 
                a length of time that depends on the size of the female and the abundance of females in the local 
                population, and this fits reasonably well with what we would predict given the fitness gained 
                from staying in copula longer or leaving to search for a new female. For this to have evolved via 
                selection, at some point there must have been among-individual variation reflecting genetic variation 
                for multidimensionality. In other words, one individual probably had a different plane than another. 
                In the module labelled <i>",Module_titles$mod5,"</i>, you visualized individuals who had 
                reaction norm planes that differed by intercept. Might they differ in slope as well? And, in which 
                dimensions? If you have done the module on random regression (we do highly recommend doing the 
                tutorial on random regression first), you will have some idea of what individual variation in slope means, 
                but extending it to 2 or more dimensions adds some complexity. Here we take you through the process of 
                adding individual variation to different bits of the equation describing a reaction norm plane."))),

  # Exercise
  p(HTML(paste0("<b>Exercise:</b> Let’s start with the equation describing MDPP that ended the 
                module called <i>",Module_titles$mod5,"</i>."))),

  # Equation 1
  p(paste0("$$",
           NOT$trait.1,"_{",NOT$time,NOT$ind,"} =
  				 
  				 ",EQ1$mean0," +
  				 ",EQ1$dev0,"  +
  				 ",EQ1$mean1,NOT$env,"_{1",NOT$time,NOT$ind,"} +
  				 ",EQ1$mean2,NOT$env,"_{2",NOT$time,NOT$ind,"} +
     			 ",EQ1$mean12,NOT$env,"_{1",NOT$time,NOT$ind,"}",NOT$env,"_{2",NOT$time,NOT$ind,"} +
  				 
  				 ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  p(paste0("This equation describes a population average plane in the space described by 
           $",NOT$env,"_{1}$ and $",NOT$env,"_{2}$ with individuals appearing like a stack of paper—individuals 
           differ only in the intercept, which is defined as the point where both 
           $",NOT$env,"_{1}$ and $",NOT$env,"_{2}$ are 0. Whether the plane is warped ($",EQ1$mean12,"\\neq0$) or not, 
           all individuals have the same planar shape. If they varied in 
           some aspect of this plane, we would not detect that in this equation.")),
  
  p(paste0("Let's add the lessons you learned about random regression in that module to this equation. 
    There, you learned that individuals might also vary in their slope with respect to $",NOT$env,"_{1}$, and 
    that individual slope could covary with individual intercept both positively and negatively. 
    What happens if that exists in the equation above?")),
  
  p("The equation becomes:"),
  
  p(paste0("$$",
           NOT$trait.1,"_{",NOT$time,NOT$ind,"} =
           
           ",EQ1$mean0," +
           ",EQ1$dev0,"  +
           (",EQ1$mean1,"+",EQ1$dev1,")",NOT$env,"_{1",NOT$time,NOT$ind,"} +
           ",EQ1$mean2,NOT$env,"_{2",NOT$time,NOT$ind,"} +
           ",EQ1$mean12,NOT$env,"_{1",NOT$time,NOT$ind,"}",NOT$env,"_{2",NOT$time,NOT$ind,"} +
           
           ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  p("where "),
  
# 	# Input: Number of individuals
#   getSliderInput("Mod5Step1_NI", Modules_VAR$NI),
# 	
# 	# Input: Among-individual variance (Vi)
#   getSliderInput("Mod5Step1_Vi", Modules_VAR$Vi),
# 	
# 	# Input: NMeasurement error variance
#   getSliderInput("Mod5Step1_Ve", Modules_VAR$Vm),
# 	
# 	# Input: Number of trait expression measured
#   getSliderInput("Mod5Step1_NR", Modules_VAR$NR),
# 	
# 	
# 	p("Note: For now, we will assume all individuals are sampled equally often and at the same time."),
# 	
# 	p(strong("The environment")),
# 	
# 	p(HTML(paste0("Let's simulate phenotypes that are influenced by two factors, both of which are shared by the 
# 		whole population (e.g., spring temperature) and the values are random from one measurement period 
# 		to the next. The environment thus has an intercept effect on phenotype of 0, and a slope that 
# 		you can input (we recommend at first that the slope be >> 0). Each environment contributes 
# 		the value $[slope]^2Var(",NOT$env,")$ to the total phenotypic variance (see Table 1 in Allegue <i>et al.</i> 2016), 
# 		so by specifying the slope (positive or negative), you will affect the total phenotypic variance. 
# 		Note that in SQuID each environmental effect is standardized (i.e. mean = 0 and variance = 1)."))),
# 	
# 	p("Enter the slope for each environmental factor. These can be either positive or negative. "),
# 	
# 	# Input: Environment 1
#   getSliderInput("Mod5Step1_B1", Modules_VAR$B1.1),
# 	
# 	# Input: Environment 2
#   getSliderInput("Mod5Step1_B2", Modules_VAR$B2.1),
# 	
# 	p(),
# 	# Simulation run button
# 	actionButton("Mod5Step1_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
# 	runningIndicator(),
# 	p(),
# 	
# 	p(strong("Results")),
# 	
# 	p("Suppose we assume that there is only one environmental effect. That is, we analyse 
# 		the population we simulated using the following model:"),
# 	
# 	# Equation
# 	p(paste0("$$",
# 							NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
# 							
# 							",EQ1$mean0," +
# 							",EQ1$dev1,"  +
# 							",EQ1$mean1,NOT$env,"_{1",NOT$time,NOT$ind,"} +
# 						  ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
# 	
# 	p("A mixed statistical model estimates the parameters:"),
# 	
# 	p("Statistical output:"),
# 	
# 	# Output: Table 1
#   uiOutput("Mod5Step1_summary_table1"),
# 	
# 	p(HTML(paste0("This makes the simple point, also made in Module <i>",Module_titles$mod3,"</i>, 
# 		that leaving out an important factor inflates other variance components. In this case 
# 		it was mostly the residual variance because the environment was set as random from one 
# 		measurement to the next and all individuals experienced it."))),
# 	
# 	p("A reanalysis with the following model pulls the missing environmental variance out of the residual term:"),
# 	
# 	# Equation
# 	p(paste0("$$",
# 					 NOT$trait.1,"_{",NOT$time,NOT$ind,"} = 
# 					 
# 					 ",EQ1$mean0," +
# 					 ",EQ1$dev1,"  +
# 
# 					 ",EQ1$mean1,NOT$env,"_{1",NOT$time,NOT$ind,"} +
# 					 ",EQ1$mean2,NOT$env,"_{2",NOT$time,NOT$ind,"} +
# 
# 					 ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
# 	
# 	p("Statistical output:"),
# 	
# 	# Output: Table 2
#   uiOutput("Mod5Step1_summary_table2"),
# 	
# 	p("This is a multiple regression within a mixed model. A 3-dimensional graph helps visualize 
# 		the way in which the two x variables affect a phenotype in the 2 dimensions defined by the environment."),
# 	
# 	# Output: Figure 1
#   uiOutput("Mod5Step1_3D_1"),
#   
# 	p(paste0("Individuals in this simulation vary in their intercept by the amount you entered. 
# 		Below we pick three individuals across the range of the intercept variance to illustrate 
# 		how each individual's plane sits in the space defined by the two environmental variables. 
# 		You can see that the three planes are parallel or very close to parallel, and differ only 
# 		in their elevation. If you play around with the number of measures within an individual, 
# 		you will see that the resolution of these planes requires fairly large sample sizes 
# 		(this is covered in more detail later).")),
# 	
# 	p(paste0("A new 3-D graph with three individuals picked from the low end of $",NOT$devI,"$, 
# 					 the middle, and from the high end of $",NOT$devI,"$.")),
#   
#   # Output: Figure 2
#   uiOutput("Mod5Step1_3D_2"),
# 	
# 	p(paste0("Run through this simulation several times using different values for $",EQ1$mean1,"$ and $",EQ1$mean2,"$, 
# 					 including having some slopes negative. In particular, try making the two have opposite signs.
# 					 Inspect the table above and look at the two graphs so you gain a feel for how the two slopes 
# 					 produce a flat plane that may be tilted in various ways.")),
# 	
#   p(HTML("<b>Conclusion:</b> is exercise should reinforce your understanding of where measured 
#   			 and unmeasured sources of variance end up in a statistical analysis and how systematic 
#   			 effects of multiple environments can be appropriately captured. In the next step, 
#   			 we illustrate one important complexity.")),
#   
#   p(strong("References:")),
#   p(HTML("Allegue, H., Araya-Ajoy, Y.G., Dingemanse, N.J., Dochtermann N.A., Garamszegi, L.Z., Nakagawa, S., Reale, D., Schielzeth, H. 
#   			 and Westneat, D.F. (2016). SQuID - Statistical Quantification of Individual Differences: an educational and 
#   			 statistical tool for understanding multi-level phenotypic data in linear mixed models. 
#   			 <i>Methods in Ecology and Evolution</i>, 8, 257-267.")),
  

    div(class = "line")
)