# UI: Module 8 Step 2
span( 
  
	# Text: title
  h4("Step 2: Adding the random interaction term"),

  conditionalPanel(
    condition = "0",
    uiOutput("Mod8Step2_hidden")
  ),
  
  p(paste0("Step 1 above layered on the random slopes for each $",NOT$env,"$ variable, 
           and these served to tip the population mean plane in various directions in environmental space, 
           much like a circus performer spinning plates at various heights and with variable skill. 
           However, the population mean plane could be warped due to an interaction between 
           the two $",NOT$env,"$ variables. Because it appears that such warping is adaptive in real organisms, 
           this would imply that individual variation in the extent of warping is possible. 
           Here we add that last random effect term to the phenotypic equation.")),


  p(paste0("This now expands the phenotypic equation to include three slope terms, $",EQ1$dev1,"$, 
           $",EQ1$dev2,"$, and $",EQ1$dev12,"$, which are the individual deviation from the population 
           slope with respect to $",NOT$env,"_{1}$, $",NOT$env,"_{2}$, and the interaction between the 
           two $",NOT$env,"$ variables, respectively. The full equation is thus:")),
  
  # equation
  p(paste0("$$",
           NOT$trait.1,"_{",NOT$time,NOT$ind,"} =
           
           ",EQ1$mean0," +
           ",EQ1$dev0,"  +
           (",EQ1$mean1,"+",EQ1$dev1,")",NOT$env,"_{1",NOT$time,NOT$ind,"} +
           (",EQ1$mean2,"+",EQ1$dev2,")",NOT$env,"_{2",NOT$time,NOT$ind,"} +
           (",EQ1$mean12,"+",EQ1$dev12,")",NOT$env,"_{1",NOT$time,NOT$ind,"}",NOT$env,"_{2",NOT$time,NOT$ind,"} +
           
           ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  p("As above, the addition of another random effect expands the variance-covariance once again. 
    It now is an ugly beast, but if you move through it systematically, you will see it is just 
    a ledger sheet that accounts for every possibility:"),
  
  p(paste0("$$",
           
           "\\begin{pmatrix}
            ",EQ1$dev0," \\\\ ",EQ1$dev1," \\\\ ",EQ1$dev2," \\\\ ",EQ1$dev12,"
            \\end{pmatrix}",
           
           "\\sim MVN(0, \\Omega_{",NOT$devI,NOT$devS,"}):  
           
           \\Omega_{",NOT$devI,NOT$devS,"}= ",
           
           "\\begin{pmatrix}
            Var(",NOT$devI,")            & Cov_{",NOT$devI,EQ3$dev1,"}  & Cov_{",NOT$devI,EQ3$dev2,"}  & Cov_{",NOT$devI,EQ3$dev12,"} \\\\ 
            Cov_{",NOT$devI,EQ3$dev1,"}  & Var(",EQ3$dev1,")            & Cov_{",EQ3$dev1,EQ3$dev2,"}  & Cov_{",EQ3$dev1,EQ3$dev12,"} \\\\ 
            Cov_{",NOT$devI,EQ3$dev2,"}  & Cov_{",EQ3$dev1,EQ3$dev2,"}  & Var(",EQ3$dev2,")            & Cov_{",EQ3$dev2,EQ3$dev12,"} \\\\
            Cov_{",NOT$devI,EQ3$dev12,"} & Cov_{",EQ3$dev1,EQ3$dev12,"} & Cov_{",EQ3$dev2,EQ3$dev12,"} & Var(",EQ3$dev12,") 
            \\end{pmatrix}",
           
           "$$")),
  
  p("We will simulate data with these terms, and assess two consequences of random slopes 
    in two dimensions. First, we will ask where variation due to the interaction term 
    ends up in a model that lacks that term. Second, we will try to visualize where 
    variation caused by variation in warping is more likely to be seen."),
  
  p(paste0("As the phenotypic equation is getting more complex, we will increase the number of sampled individuals to 500. 
           As before, each individual is measured 20 times for both $",NOT$env,"$ variables and both environments are random and unshared.")),
  
  p("Below, specify some parameter values:"),

  # input -------------------------------------->>>>>>>>>>>>>>>>>>>>>>>>>>>
  
  # Input: Among-individual variance in intercept (Vi)
  getSliderInput("Mod8Step2_Vi", Modules_VAR$Vi),
  
  # Input: Measurement error variance
  getSliderInput("Mod8Step2_Ve", Modules_VAR$Vm),
  
  # Input: Environment 1
  getSliderInput("Mod8Step2_B1", Modules_VAR$B1.1),
  
  # Input: Environment 2
  getSliderInput("Mod8Step2_B2", Modules_VAR$B2.1),
  
  # Input: Environment 12
  getSliderInput("Mod8Step2_B12", Modules_VAR$B1122),
  
  # Input: Among-individual variance in slope (Vs)
  getSliderInput("Mod8Step2_Vs1", Modules_VAR$Vsx.1),
  
  # Input: Among-individual variance in slope (Vs)
  getSliderInput("Mod8Step2_Vs2", Modules_VAR$Vsx.2),
  
  # Input: Among-individual variance in slope (Vs)
  getSliderInput("Mod8Step2_Vs12", Modules_VAR$Vsx.12),
  
  # Input: Correlation between Vi and Vs1
  getSliderInput("Mod8Step2_CorIS1", Modules_VAR$CorIS1),
  
  # Input: Correlation between Vi and Vs2
  getSliderInput("Mod8Step2_CorIS2", Modules_VAR$CorIS2),
  
  # Input: Correlation between Vi and Vs2
  getSliderInput("Mod8Step2_CorIS12", Modules_VAR$CorIS12),
  
  # Input: Correlation between Vs1 and Vs2
  getSliderInput("Mod8Step2_CorS1S2", Modules_VAR$CorS1S2),
  
  # Input: Correlation between Vs1 and Vs12
  getSliderInput("Mod8Step2_CorS1S12", Modules_VAR$CorS1S12),
  
  # Input: Correlation between Vs2 and Vs12
  getSliderInput("Mod8Step2_CorS2S12", Modules_VAR$CorS2S12),
  

  p("As before, it might be easiest to start with 0 covariances (represented as correlations) and add them in 
    individually so you can more easily see what each does."),
  
  p("Once the data are simulated, we can analyze them with lme4 as was done in 
    the random regression module. For example, we can use the following equation 
    in which the individual specific interaction term is omitted:"),
  
  p(paste0("$$",
           NOT$trait.1,"_{",NOT$time,NOT$ind,"} =
           
           ",EQ1$mean0," +
           ",EQ1$dev0,"  +
           (",EQ1$mean1,"+",EQ1$dev1,")",NOT$env,"_{1",NOT$time,NOT$ind,"} +
           (",EQ1$mean2,"+",EQ1$dev2,")",NOT$env,"_{2",NOT$time,NOT$ind,"} +
           (",EQ1$mean12,")",NOT$env,"_{1",NOT$time,NOT$ind,"}",NOT$env,"_{2",NOT$time,NOT$ind,"} +
           
           ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  displayRCode("# install.packages(&quot;lme4&quot;)<br>
                LMM1 <- lme4::lmer(Phenotype ~ 1 + X1*X2 (1 + X1 + X2|Individual) 
                                   + (0 + X2|Individual), data = sampled_data)"),
  
  p("And we will compare those results with the full model:"),
  
  p(paste0("$$",
         NOT$trait.1,"_{",NOT$time,NOT$ind,"} =
           
           ",EQ1$mean0," +
           ",EQ1$dev0,"  +
           (",EQ1$mean1,"+",EQ1$dev1,")",NOT$env,"_{1",NOT$time,NOT$ind,"} +
           (",EQ1$mean2,"+",EQ1$dev2,")",NOT$env,"_{2",NOT$time,NOT$ind,"} +
           (",EQ1$mean12,"+",EQ1$dev12,")",NOT$env,"_{1",NOT$time,NOT$ind,"}",NOT$env,"_{2",NOT$time,NOT$ind,"} +
           
           ",NOT$error,"_{",NOT$time,NOT$ind,"}$$")),
  
  
  displayRCode("LMM2 <- lme4::lmer(Phenotype ~ 1 + X1*X2 (1+X1*X2|Individual), data = sampled_data)"),
  
  # Simulation run button
  actionButton("Mod8Step2_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
  runningIndicator(),
  sim_msg(),
  
  p("Statistical output:"),
  
  # output: table
  uiOutput("Mod8Step2_summary_table"),
  
  p("As usual, omission of a key parameter causes variation to be placed elsewhere in the equation, 
    in this case mostly in the residual."),
  
  p(paste0("A 3-d plot of the population mean (below) provides you with a visual orientation 
           to the average phenotype across the environmental space created by the two $",NOT$env,"$ variables. 
           We have also plotted the values different individuals will express at the corners of 
           the graph where they would experience an extreme in both $",NOT$env,"$ distributions.")),

  # output: figure ----------------
  p(plotlyOutput("Mod8Step2_3D")),

  p("Run this simulation several times with different values for the interaction term 
    and the covariance terms. Where does the interaction term create the most phenotypic 
    variance and how do covariances affect this?"),
    
  # Conclusion
  p(HTML("<b>Conclusion:</b> The effect of interactions between environments on phenotypes 
         has three important characteristics. First, it seems biologically likely given 
         the complexity of the environment and it in fact exists in many traits. 
         Second, these effects can be modelled using mixed models, including the random effects 
         of individual on the response to each environment and in theory on the interaction term itself. 
         The third characteristic is that these models are exceedingly complex. At this point, you 
         don't have in your mental pocket the full phenotypic equation. There are many more 
         complexities to explore, but you now have all the basic tools. 
         The SQuID platform can now be explored so you can assess what sampling 
         regimes and experimental designs will allow you to effectively 
         measure the attributes of most interest.")),
  
  div(class = "line"),
  
  actionLink("Mod8Step2GotoStep1", label = "<< Previous Step (1)", class= "linkToModuleSteps"), # Go to previous step

)