# UI: Module 4 Step 2
span( 
  
	# Text: title
  h4("Step 2: Phenotypic correlations between two repeatedly expressed traits 
  	 that also differ consistently between individuals."),

  # Sub-goal
  p(HTML("<b>Sub-goal:</b> Understanding the role of repeatability in shaping phenotypic 
         correlations in repeatedly expressed traits")),

  conditionalPanel(
    condition = "0",
    uiOutput("Mod4Step2_hidden")
  ),

  
  # Introduction
	p(HTML(paste0("<b>Introduction:</b> In step one, we focussed on within-individual correlations caused by trade-offs 
								between two costly traits that were repeatedly expressed within the same individual. 
								We assumed for simplicity that both traits harboured no among-individual variance. 
								This meant that the individual repeatability was zero for both traits 
								(see module on <i>",Module_titles$mod1,"</i> for an introduction to repeatability). 
								We will now consider a more complex but realistic scenario where these two traits (egg size and egg number) 
								varied both within- and among-individuals, as we know is the case in natural populations.
								The models for this step will be:"))),

	### Model equations
	p(paste0("$$",NOT$trait.1,"_{",NOT$time,NOT$ind,"} = ",NOT$devI,"_{",NOT$trait.1,NOT$ind,"} + 
																											 ",NOT$error,"_{",NOT$trait.1,NOT$time,NOT$ind,"}$$
  				  $$",NOT$trait.2,"_{",NOT$time,NOT$ind,"} = ",NOT$devI,"_{",NOT$trait.2,NOT$ind,"} + 
					 																						 ",NOT$error,"_{",NOT$trait.2,NOT$time,NOT$ind,"}$$")),
	
	p(HTML(paste0("For example, female passerine birds often adjust their clutch size to changes in breeding 
						density while females also differ consistently in clutch sizes across repeated breeding attempts 
						(Nicolaus <i>et al.</i> 2013). Previously, because the traits did not harbour any among-individual variation, 
						all variation in the data existed at within-individuals. This meant that the phenotypic correlation 
						between the two traits in the dataset was identical to the within-individual correlation. 
						We can see why this is the case by introducing the equation that describes the components affecting 
						the phenotypic correlation ($r_{",NOT$total,"_",NOT$trait.1,",",NOT$total,"_",NOT$trait.2,"}$) 
						(e.g. Dingemanse & Dochtermann 2013; Dingemanse <i>et al.</i> 2012 ):"))),

  
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
  
  p(paste0("This monster contains several important elements. The phenotypic correlation 
  				 ($r_{",NOT$total,"_",NOT$trait.1,",",NOT$total,"_",NOT$trait.2,"}$) 
  				 is affected by both the among-individual correlation ($r_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}$) 
  				 and the within-individual correlation ($r_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$). 
  				 This former correlation will be addressed in more detail in step 3 of this module. 
  				 Important for now is to note that the impact of each correlation on the phenotypic correlation is weighted 
  				 by the following term:")),
  
  
  ####### eq 2 
  p(paste0("$$ 
  				 \\sqrt{
           (\\frac{V_{",NOT$devI,"_" ,NOT$trait.1,"}}
  				 {V_{",NOT$devI,"_" ,NOT$trait.1,"} + V_{",NOT$error,"_" ,NOT$trait.1,"}})
  				 (\\frac{V_{",NOT$devI,"_" ,NOT$trait.2,"}}
  				 {V_{",NOT$devI,"_" ,NOT$trait.2,"} + V_{",NOT$error,"_" ,NOT$trait.2,"}})}
  				 $$")),
  
  p(HTML(paste0('This term is called the "geometric mean repeatability" as it represents the square-root of product 
								of the repeatability of trait ',NOT$trait.1,' $(\\frac{V_{',NOT$devI,'_' ,NOT$trait.1,'}}
								{V_{',NOT$devI,'_',NOT$trait.1,'} + V_{',NOT$error,'_',NOT$trait.1,'}})$ 
								and trait ',NOT$trait.2,' $(\\frac{V_{',NOT$devI,'_' ,NOT$trait.2,'}}
								{V_{',NOT$devI,'_',NOT$trait.2,'} + V_{',NOT$error,'_',NOT$trait.2,'}})$ .
  				 			Note that the term $\\sqrt{
			           (\\frac{V_{',NOT$error,'_',NOT$trait.1,'}}
			           {V_{',NOT$devI,'_',NOT$trait.1,'} + V_{',NOT$error,'_',NOT$trait.1,'}})
			           (\\frac{V_{',NOT$error,'_',NOT$trait.2,'}}
			           {V_{',NOT$devI,'_',NOT$trait.2,'} + V_{',NOT$error,'_',NOT$trait.2,'}})}$
								essentially represents one minus this geometric repeatability 
  							and that both terms add up to the value of 1. This means that for two traits that 
  							are highly repeatable (e.g. sets of morphological traits), the phenotypic correlation 
  							will largely reflect the among-individual correlation, whereas for two traits that have 
  							a low repeatability (e.g. sets of behavioural traits), the phenotypic correlation instead 
  							largely reflects the within-individual correlation (Dingemanse & Dochtermann 2013).'))),
  
  p("Let us now return to the example dataset that we considered in Step 1, where we focussed on two 
  	repeatedly expressed traits that were negatively correlated within-individuals but did 
  	not harbour any among-individual variance."),
  
  p("We reprint the equation describing the components shaping the phenotypic correlation:"),
  
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
  
  p(paste0("We have automatically taken forward your inputs from step 1, 
  				 where you have set the within-individual variances for traits y and z as well as their 
  				 covariance but where you assumed that the among-individual variances 
  				 ($V_{",NOT$devI,"_",NOT$trait.1,"}$, $V_{",NOT$devI,"_",NOT$trait.2,"}$) were equal to zero:")),

  ######## Equation of phenotopic correlation
  uiOutput("Mod4Step2_Phenotopic_correlation"),
  
  p("What you should note is that the geometric mean repeatability (defined above) is equal to zero, 
  	while one minus this metric is equal to one, in cases such as these where at least 
  	one of the traits harbours no among-individual variance; therefore:"),
  
  p("$$r_{",NOT$total,"_",NOT$trait.1,",",NOT$total,"_",NOT$trait.2,"} = 
  	r_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$$"),
  
  p(HTML(paste0(strong("Exercise: "),"We will now introduce among-individual variance in both egg size 
  							(i.e., $V_{",NOT$devI,"_",NOT$trait.1,"}>0$) and clutch size (i.e., $V_{",NOT$devI,"_",NOT$trait.1,"}>0$). 
  							For simplicity, we will assume that an individual’s average egg size over all its repeated observations 
  							is not correlated with its average clutch size over all its repeated observations, i.e., 
  							that the among-individual correlation is zero."))),
  
  p("As in step 1 above, we have set both the number of individuals, and the number of repeated measures per individual, to 10."),
  
  p("Set the amount of within-individual variance in each of the two traits:"),
  # Within-individual variances (Ve)
  fluidRow(
  	column(6,getSliderInput("Mod4Step2_Ve1", Modules_VAR$Ve1)),
  	column(6,getSliderInput("Mod4Step2_Ve2", Modules_VAR$Ve2))
  ),
  
  p("Set the within-individual correlation, which should be negative in case females trade-off their investment in egg size versus number:"),
  # Within-individual correlation
  getSliderInput("Mod4Step2_Corr_e", Modules_VAR$Corr_e),
  
  p("Previously, we assumed that the among-individual variances in y and z were zero, now set them to some number:"),
  fluidRow(
  	column(6,getSliderInput("Mod4Step2_Vi1", Modules_VAR$Vi1)),
  	column(6,getSliderInput("Mod4Step2_Vi2", Modules_VAR$Vi2))
  ),
  
  p("Remember, we assume for now that the among-individual correlation is zero."),
  
  p(strong("Results:")),
  
  p('As we have seen in step 1, you have defined the elements of the within-individual "variance-covariance matrix":'),

  
  p("Your entries resulted in the following values for this matrix"),
  
  ######## Matrix with the values entered
  uiOutput("Mod4Step2_Within_Covariance_Matrix"),
  
  p("You have now also defined the values for the among-individual matrix:"),
  p(paste0(
  	"$$ \\Omega_{",NOT$devI,"}=
  	\\begin{pmatrix}
  	V_{",NOT$devI,"_",NOT$trait.1,"} & Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}  \\\\
  	Cov_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"} & V_{", NOT$devI,"_",NOT$trait.2,"} \\\\
  	\\end{pmatrix} 
  	$$")),
  
  p("Your entries resulted in the following values for this matrix"),
  
  ######## Matrix with the values entered
  uiOutput("Mod4Step2_Among_Covariance_Matrix"),
  
  p(paste0("As you have set both the among- and within-individual variances, you have defined the repeatabilities of both traits. 
  	As detailed in module on ",Module_titles$mod1,", repeatability ranges between 0 and 1 and is calculated as:")),
  
  p(paste0("$$Repeatability=\\frac{V'_",NOT$devI,"}{V'_",NOT$devI,"+V'_",NOT$error,"}$$")),
  
  p("The repeatabilities of your two traits are:"),
  
  ######## Matrix with the values entered
  uiOutput("Mod4Step2_Repeatabilities"),
  
  p("We will now return to the equation that describes the components that shape the phenotypic correlation:"),
  ####### eq 1
  p(paste0("$$
           r_{",NOT$total,"_",NOT$trait.1,",",NOT$total,"_",NOT$trait.2,"} = 
           
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
  
  uiOutput("Mod4Step2_Phenotopic_correlation2"),
  
  p(paste0("Note that the phenotypic correlation ($r_{",NOT$total,"_",NOT$trait.1,",",NOT$total,"_",NOT$trait.2,"}$)
  				 no longer matches the within-individual correlation ($r_{",NOT$error,"_",NOT$trait.1,",",NOT$error,"_",NOT$trait.2,"}$). 
  				 This is because both traits now harbour non-zero repeatability; as we have set the among-individual correlation 
  				 ($r_{",NOT$devI,"_",NOT$trait.1,",",NOT$devI,"_",NOT$trait.2,"}$) to zero, in this specific scenario, 
  				 we can simplify the equation into:")),
  
  ###### Equation
  p(paste0("$$
           r_{",NOT$total,"_",NOT$trait.1,",",NOT$total,"_",NOT$trait.2,"} =

           r_{",NOT$error,"_" ,NOT$trait.1,",",NOT$error,"_" ,NOT$trait.2,"}
           \\sqrt{
           (\\frac{V_{",NOT$error,"_" ,NOT$trait.1,"}}
           {V_{",NOT$devI,"_" ,NOT$trait.1,"} + V_{",NOT$error,"_" ,NOT$trait.1,"}})
           (\\frac{V_{",NOT$error,"_" ,NOT$trait.2,"}}
           {V_{",NOT$devI,"_" ,NOT$trait.2,"} + V_{",NOT$error,"_" ,NOT$trait.2,"}})}
           $$")),
  
  
  p("Doing so clarifies that the geometric mean repeatability of the two traits defines how well 
  	the phenotypic correlation in the data corresponds to the within-individual correlation occurring 
  	because of the presumed trade-off between investments in egg size vs. number. 
  	The equation also clarifies that the more repeatable the two traits are, the less apparent is the 
  	trade-off if one would focus solely on the overall phenotypic correlation."),
  p("This insight can be visualized by presenting your data in two scatter plots."),
  p("The first scatter plot shows the correlation between the two traits that your entries 
  	(of within- and among-individual variances and covariances) above produced:"),
  
  # Simulation run button
  actionButton("Mod4Step2_Run", label = Modules_VAR$Run$label, icon = Modules_VAR$Run$icon, class = "runButton"),
  runningIndicator(),
  sim_msg(),
  
  plotOutput("Mod4Step2_correlationplot", width = Modules_VAR$Plot$width),
  
  p("The second scatter plot also uses your entries but assumes that you entered zero among-individual variances. 
  	This second plot therefore depicts the correlation in your data at the within-individual level alone:"),
  
  p(),
  plotOutput("Mod4Step2_correlationplot2", width = Modules_VAR$Plot$width),
  
  p("The first graph should show a weaker overall phenotypic association as explained above."),
  
  p(HTML("<b>Conclusion:</b> The phenotypic correlation reflects the within-individual correlation when 
  			 measured for repeatedly expressed traits; however, their correspondence is a function of how 
  			 repeatable the two traits are. You may explore this idea more fully by re-running the exercise 
  			 for different levels of trait repeatabilities.")),
  
  p(strong("References:")),
  p(HTML("Dingemanse, N.J. & Dochtermann, N.A. (2013) Quantifying individual variation in behaviour: 
         mixed-effect modelling approaches. <i>Journal of Animal Ecology</i>, 82, 39-54.
         <a href='https://doi.org/10.1111/1365-2656.12013' target='_blank'>doi: 10.1111/1365-2656.12013</a>")),
  p(HTML('Dingemanse, N.J., Dochtermann, N.A. & Nakagawa, S. (2012) Defining behavioural syndromes and the role of 
         "syndrome deviation" to study its evolution. <i>Behavioral Ecology and Sociobiology</i>, 66, 1543-1548.
         <a href="https://doi.org/10.1007/s00265-012-1416-2" target="_blank">doi: 10.1007/s00265-012-1416-2</a>')),
  p(HTML("Nicolaus, M., Brommer, J.E., Ubels, R., Tinbergen, J.M. & Dingemanse, N.J. (2013) 
          Exploring patterns of variation in clutch size-density reaction norms in a wild passerine bird. 
          <i>Journal of Evolutionary Biology</i>, 26, 2031-2043.
          <a href='https://doi.org/10.1111/jeb.12210' target='_blank'>doi: 10.1111/jeb.12210</a>")),
	
	div(class = "line"),
	
	actionLink("Mod4Step2GotoStep1", label = "<< Previous Step (1)", class = "linkToModuleSteps"), # Go to previous step       
	span(Modules_VAR$StepLink$sep, class = "step-Link"),
	actionLink("Mod4Step2GotoStep3", label = "Next Step (3) >>", class = "linkToModuleSteps") # Go to next step
)