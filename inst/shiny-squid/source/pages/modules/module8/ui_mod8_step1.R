# UI: Module 8 Step 1
span( 
  
	# Text: title
  h4("Step 1: Adding multiple slopes that vary among individuals"),

  # Sub-goal
  p(HTML("<b>Sub-goal:</b> Can we statistically test for individual variation in interaction 
         terms and what sampling regimes might improve the ability to do so?")),

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
  p(HTML(paste0("<b>Exercise:</b> Let's start with the equation describing MDPP that ended the 
                module called <i>",Module_titles$mod5,"</i>."))),

  # Equation
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
  
  p(paste0("$$",

           "\\begin{pmatrix}
            ",EQ1$dev0," \\\\ ",EQ1$dev1,"
            \\end{pmatrix}",
           
           "\\sim MVN(0, \\Omega_{",NOT$devI,NOT$devS,"}):  
           
           \\Omega_{",NOT$devI,NOT$devS,"}= ",
           
           "\\begin{pmatrix}
            Var(",NOT$devI,") & Cov_{",NOT$devI,EQ3$dev1,"} \\\\ 
            Cov_{",NOT$devI,EQ3$dev1,"} & Var(",EQ3$dev1,")
            \\end{pmatrix}",

           "$$")),

  p(paste0("If you did the random regression module, you have seen this before although here we've added 
           one subscript to identify that the slopes are with respect to $",NOT$env,"_{1}$. 
           This statement means that the intercept value of the $",NOT$ind,"^{th}$ individual ($",EQ1$dev0,"$) 
           and the slope of that individual ($",EQ1$dev1,"$) with respect to $",NOT$env,"_{1}$ are 
           distributed as multivariate normal with means for each attribute of 0 and a variance-covariance 
           structure of $\\Omega_{",NOT$devI,NOT$devS,"}$. We then specify the variance-covariance structure 
           in a 2 by 2 matrix. $Cov_{",NOT$devI,EQ3$dev1,"}$ is the measure of how much $",EQ1$dev0,"$ and 
           $",EQ1$dev1,"$ covary together.")),
  
  
  p(paste0("We will simulate data with these terms and assess one consequence of random slopes in a reaction 
           norm plane. First, we will ask where variation due to the random slope exists in the 2 environmental 
           dimensions.")),
  
  p("We will start with a population of 100 individuals each measured 20 times in which both 
           x variables were also measured. Both environments are random and unshared."),

  p("Below, specify some parameter values:"),
  
  info_msg("Note that covariance is entered as correlation (i.e. standardized covariance) ranging from -1 to 1."),
  
  # input -------------------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  conditionalPanel(
    condition = "0",
    uiOutput("Mod8Step1_hidden")
  ),
  
  # Input: Among-individual variance in intercept (Vi)
  getSliderInput("Mod8Step1_Vi", Modules_VAR$Vi),
  
  # Input: Measurement error variance
  getSliderInput("Mod8Step1_Ve", Modules_VAR$Vm),
  
  # Input: Environment 1
  getSliderInput("Mod8Step1_B1", Modules_VAR$B1.1),
  
  # Input: Environment 2
  getSliderInput("Mod8Step1_B2", Modules_VAR$B2.1),
  
  # Input: Environment 12
  getSliderInput("Mod8Step1_B12", Modules_VAR$B1122),
  
  # Input: Among-individual variance in slope (Vs)
  getSliderInput("Mod8Step1_Vs1", Modules_VAR$Vsx.1),
  
  # Input: Correlation between Vi and Vs1
  getSliderInput("Mod8Step1_CorIS1", Modules_VAR$CorIS1),
  
  p(paste0("It may be useful at first to make the $Cov_{",NOT$devI,EQ3$dev1,"}=0$, 
           but later you can explore other values.")),
  
  p(paste0("Once the data are simulated, we can analyze them with R package lmer4 
           as was done in the random regression module and we will compare the full model 
           to one without $",EQ1$dev1,"$ or $Cov_{",NOT$devI,EQ3$dev1,"}$.")),
  
  displayRCode("# install.packages(&quot;lme4&quot;)<br>
                LMM1 <- lme4::lmer(Phenotype ~ 1 + X1*X2 (1 + X1|Individual), data = sampled_data)<br>
                LMM2 <- lme4::lmer(Phenotype ~ 1 + X1*X2 (1|Individual), data = sampled_data)"),
  
  # Simulation run button
  actionButton("Mod8Step1_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
  runningIndicator(),
  sim_msg(),
  
  p("Statistical output:"),
  
  # Output: Table 1
  uiOutput("Mod8Step1_summary_table1"),
  
  p("The above should illustrate again that leaving something out of an analysis model that exists 
    in the data will produce errors in other terms."),
  
  p("To illustrate what variance the random slopes capture, look at the following graphs:"),
  
  # Figure 1
  p(plotlyOutput("Mod8Step1_3D_1")),
  
  p(paste0("A key feature is that while there is a plane that describes the reaction norm in space defined 
           by $",NOT$env,"_{1}$ and $",NOT$env,"_{2}$, the random slopes for $",NOT$env,"_{1}$ are measured 
           in only one value of $",NOT$env,"_{2}$, where it is 0. Below we present another graph with 
           the reaction norm planes of 3 individuals picked from the data.")),
  
  # Figure 2
  p(plotlyOutput("Mod8Step1_3D_2")),
  
  p(paste0("As you can see, these planes vary in only one dimension of the environmental space. 
           Since we did not specify any variation in the reaction to $",NOT$env,"_{2}$, or to the interaction between 
           $",NOT$env,"_{1}$ and $",NOT$env,"_{2}$, everyone is assumed to have the same plane in those directions.")),
  
  p(paste0("If you try this with several different values for the parameters 
           $V_{",NOT$devI,"}$, $V_{",EQ3$dev1,"}$, and $Cov_{",NOT$devI,EQ3$dev1,"}$, 
           you will see that these affect the orientation of the plane only in one dimension.")),
  
  
  # Exercise
  p(HTML(paste0("<b>Exercise:</b> Now we will add among-individual variation in response to $",NOT$env,"_{2}$. 
                In the phenotypic equation, we simply add $",EQ1$dev2,"$ to the part that depends on $",NOT$env,"_{2}$."))),
  
  p(paste0("$$",
           NOT$trait.1,"_{",NOT$time, NOT$ind,"} =
           
           ",EQ1$mean0," +
           ",EQ1$dev0,"  +
           
           (",EQ1$mean1,"+",EQ1$dev1,")",NOT$env,"_{1",NOT$time,NOT$ind,"} +
           
           (",EQ1$mean2,"+",EQ1$dev2,")",NOT$env,"_{2",NOT$time,NOT$ind,"} +
           
           ",EQ1$mean12,NOT$env,"_{1",NOT$time,NOT$ind,"}",NOT$env,"_{2",NOT$time,NOT$ind,"} +
           
           ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  p("However, we now have a more complex variance-covariance expression, 
    which is the expansion of all possible combinations of 
    three terms instead of two as before:"),
  
  p(paste0("$$",
           
           "\\begin{pmatrix}
            ",EQ1$dev0," \\\\ ",EQ1$dev1," \\\\ ",EQ1$dev2,"
            \\end{pmatrix}",
           
           "\\sim MVN(0, \\Omega_{",NOT$devI,NOT$devS,"}):  
           
           \\Omega_{",NOT$devI,NOT$devS,"}= ",
           
           "\\begin{pmatrix}
            Var(",NOT$devI,") & Cov_{",NOT$devI,EQ3$dev1,"} & Cov_{",NOT$devI,EQ3$dev2,"} \\\\ 
            Cov_{",NOT$devI,EQ3$dev1,"} & Var(",EQ3$dev1,") & Cov_{",EQ3$dev1,EQ3$dev2,"} \\\\ 
            Cov_{",NOT$devI,EQ3$dev2,"} & Cov_{",EQ3$dev1,EQ3$dev2,"} & Var(",EQ3$dev2,")
            \\end{pmatrix}",
           
           "$$")),
  
  p("Again, let's simulate some data and visualize the results."),
  
  p(paste0("We will start with a population of 100 individuals each measured 20 times in which 
           both $",NOT$env,"$ variables were also measured. Both environments are random and unshared.")),
  
  
  p("Below, specify some parameter values:"),
  
  # input -------------------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  conditionalPanel(
    condition = "0",
    uiOutput("Mod8Step1_2_hidden")
  ),
  
  # Input: Among-individual variance in intercept (Vi)
  getSliderInput("Mod8Step1_2_Vi", Modules_VAR$Vi),
  
  # Input: Measurement error variance
  getSliderInput("Mod8Step1_2_Ve", Modules_VAR$Vm),
  
  # Input: Environment 1
  getSliderInput("Mod8Step1_2_B1", Modules_VAR$B1.1),
  
  # Input: Environment 2
  getSliderInput("Mod8Step1_2_B2", Modules_VAR$B2.1),
  
  # Input: Environment 12
  getSliderInput("Mod8Step1_2_B12", Modules_VAR$B1122),
  
  # Input: Among-individual variance in slope (Vs)
  getSliderInput("Mod8Step1_2_Vs1", Modules_VAR$Vsx.1),
  
  # Input: Among-individual variance in slope (Vs)
  getSliderInput("Mod8Step1_2_Vs2", Modules_VAR$Vsx.2),
  
  # Input: Correlation between Vi and Vs1
  getSliderInput("Mod8Step1_2_CorIS1", Modules_VAR$CorIS1),
  
  # Input: Correlation between Vi and Vs2
  getSliderInput("Mod8Step1_2_CorIS2", Modules_VAR$CorIS2),
  
  # Input: Correlation between Vs1 and Vs2
  getSliderInput("Mod8Step1_2_CorS1S2", Modules_VAR$CorS1S2),
  
  p("We might recommend you set two of these covariances to 0 to start and explore what 
    the other looks like before giving them all values. Strong covariances, either positive 
    or negative, will be easier to visualize."),
  
  p("Once the data are simulated, we can analyze them with lmer4 as was done in the random regression
    module. This time we will just fit the full model and visualize the results in a graph."),
  
  displayRCode("# install.packages(&quot;lme4&quot;)<br>
                LMM1 <- lme4::lmer(Phenotype ~ 1 + X1*X2 (1 + X1 + X2|Individual), data = sampled_data)"),
  
  # Simulation run button
  actionButton("Mod8Step1_2_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
  runningIndicator(),
  sim_msg(),
  
  p("Statistical output:"),
  
  # Output: Table 2
  uiOutput("Mod8Step1_2_summary_table1"),
  
  p("To illustrate now what variance both random slopes produce, look at the following graphs:"),
  
  # Figure 3
  p(plotlyOutput("Mod8Step1_2_3D")),
  
  p(paste0("If you try this with several different values for the parameters 
           $V_{",NOT$devI,"}$, $V_{",EQ3$dev1,"}$, and $V_{",EQ3$dev2,"}$, 
           and the 3 covariances, you will see that these affect the orientation of the plane 
           around the 0 point for both environments, and its height at that spot, 
           but any curving of the plane due to the parameter $",EQ1$mean12,"$ is the same for everyone.")),
  
  
  p("As you can see, these planes now vary in both dimensions of the environmental space. 
    If you think of the intercept as a pole in this space, the individual slopes cause 
    the planes to wobble or tip in all directions at the top of the pole relative 
    to the population mean plane and the variation in intercept causes the plane 
    to rise or sink at the intercept relative to the population mean plane. 
    What do values of the covariances do? You can think of them constraining the range 
    of plane tipping in just certain directions, and the different covariances 
    cause different constraints."),
  

  div(class = "line"),
  
  actionLink("Mod8Step1GotoStep2", label = "Next Step (2) >>", class= "linkToModuleSteps") # Go to next step
)