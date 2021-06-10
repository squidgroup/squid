navbarMenu("Modules", # Title
           icon=icon("tasks", "fa-fw"), # Icon

  ####### Module 1: Basic Lessons about Variance ###########################################
  tabPanel(Module_titles$mod1, 
    fixedPage(tags$div(class="myPage myTutorial",
      wellPanel( 
      # Title
      h3(HTML(module1_txt$title)),
      p(HTML(module1_txt$goal)), 
      tabsetPanel(id = "Module1TabsetPanel", type = "pills", selected = "Step 1",
        tabPanel("Step 1", source("./source/pages/modules/module1/ui_mod1_step1.R",local = TRUE)[["value"]]),    
        tabPanel("Step 2", source("./source/pages/modules/module1/ui_mod1_step2.R",local = TRUE)[["value"]]),
        tabPanel("Step 3", source("./source/pages/modules/module1/ui_mod1_step3.R",local = TRUE)[["value"]]),
        tabPanel("Step 4", source("./source/pages/modules/module1/ui_mod1_step4.R",local = TRUE)[["value"]])
      ) # End tabsetPanel
    ) # End Wellpanel
  ))), # END Module 1

  ####### Module 2: Non-Gaussian traits ###########################################
  tabPanel(Module_titles$mod2,
    fixedPage(tags$div(class="myPage myTutorial",
      wellPanel(
        # Title
        h3("Introduction to non-Gaussian traits (count, binary and proportion data) and generalized linear mixed-effects models"),
        p(HTML("<b>Goal:</b> to understand what kinds of traits are 'non-Gaussian',
               how they are different from Gaussian (normally distributed) traits,
               and how they can be modeled using the 'generalized' linear mixed-effects model (GLMM) framework.")),
        tabsetPanel(id = "Module2TabsetPanel", type = "pills", selected = "Step 1",
                    tabPanel("Step 1", source("./source/pages/modules/module2/ui_mod2_step1.R",local = TRUE)[["value"]]),
                    tabPanel("Step 2", source("./source/pages/modules/module2/ui_mod2_step2.R",local = TRUE)[["value"]]),
                    tabPanel("Step 3", source("./source/pages/modules/module2/ui_mod2_step3.R",local = TRUE)[["value"]])
        ) # End tabsetPanel
      ) # End Wellpanel
  ))), # END Module 2
     
  ####### Module 3: Non-stochastic environments #######################################
  tabPanel(Module_titles$mod3, 
    fixedPage(tags$div(class="myPage myTutorial",               
    wellPanel( 
      # Title
      h3(HTML(module3_txt$title)),
      p(HTML(module3_txt$goal)), 
      
      tabsetPanel(id = "Module2TabsetPanel", type = "pills", selected = "Step 1",
        tabPanel("Step 1", source("./source/pages/modules/module3/ui_mod3_step1.R",local = TRUE)[["value"]]),
        tabPanel("Step 2", source("./source/pages/modules/module3/ui_mod3_step2.R",local = TRUE)[["value"]]),
        tabPanel("Step 3", source("./source/pages/modules/module3/ui_mod3_step3.R",local = TRUE)[["value"]])
      )
    )
  ))), # END Module 3
     
  ####### Module 6: Random regressions ######################
  tabPanel(Module_titles$mod6,
    fixedPage(tags$div(class="myPage myTutorial",
    wellPanel( 
      # Title
      h3(HTML(module6_txt$title)),
      p(HTML(module6_txt$goal)), 
      tabsetPanel(id = "Module6TabsetPanel", type = "pills", selected = "Step 1",
        tabPanel("Step 1", source("./source/pages/modules/module6/ui_mod6_step1.R",local = TRUE)[["value"]]),
        tabPanel("Step 2", source("./source/pages/modules/module6/ui_mod6_step2.R",local = TRUE)[["value"]]),
        tabPanel("Step 3",source("./source/pages/modules/module6/ui_mod6_step3.R" ,local = TRUE)[["value"]])
      ) # End tabsetPanel
    ) # End Wellpanel
  ))), # END Module 6

  #######  Module 4: Multiple traits  #######################################
  tabPanel(Module_titles$mod4,
   fixedPage(tags$div(class="myPage myTutorial",
	 wellPanel(
	 	# Title
	 	h3("Multiple traits: processes influencing phenotypic correlations in repeatedly expressed traits."),
	 	p(HTML("<b>Goal:</b> to develop understanding of how phenotypic correlations between two
  				 repeatedly expressed traits are affected by the amount of variation,
  				 and the magnitude of correlations occurring at each underlying hierarchical level.")),
	 	tabsetPanel(id = "Module4TabsetPanel", type = "pills", selected = "Step 1",
	 							tabPanel("Step 1", source("./source/pages/modules/module4/ui_mod4_step1.R",local = TRUE)[["value"]]),
	 							tabPanel("Step 2", source("./source/pages/modules/module4/ui_mod4_step2.R",local = TRUE)[["value"]]),
	 							tabPanel("Step 3", source("./source/pages/modules/module4/ui_mod4_step3.R",local = TRUE)[["value"]]),
	 							tabPanel("Step 4", source("./source/pages/modules/module4/ui_mod4_step4.R",local = TRUE)[["value"]]),
	 							tabPanel("Step 5", source("./source/pages/modules/module4/ui_mod4_step5.R",local = TRUE)[["value"]])
	 	) # End tabsetPanel
	 ) # End Wellpanel
  ))), # END Module 4
   
  #######  Module 5: Multi-dimensional Phenotypic Plasticity  #######################################
  tabPanel(Module_titles$mod5,
    fixedPage(tags$div(class="myPage myTutorial",
    wellPanel(
      # Title
      h3("Multi-dimensional Phenotypic Plasticity (MDPP)"),
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
      tabsetPanel(id = "Module5TabsetPanel", type = "pills", selected = "Step 1",
      						tabPanel("Step 1", source("./source/pages/modules/module5/ui_mod5_step1.R",local = TRUE)[["value"]]),
      						tabPanel("Step 2", source("./source/pages/modules/module5/ui_mod5_step2.R",local = TRUE)[["value"]])
      ) # End tabsetPanel
    ) # End Wellpanel
  ))), # END Module 5

#          # Module 7
#          tabPanel("Module 7",UImodule7()), # END Module 7
#    

  #######  Module 8: MDPP and random slopes  #######################################
  tabPanel(Module_titles$mod8,
  fixedPage(tags$div(class="myPage myTutorial",
   wellPanel(
     # Title
     h3("Combining multidimensionality with random regression"),
     tabsetPanel(id = "Module8TabsetPanel", type = "pills", selected = "Step 1",
        tabPanel("Step 1", source("./source/pages/modules/module8/ui_mod8_step1.R",local = TRUE)[["value"]]),
        tabPanel("Step 2", source("./source/pages/modules/module8/ui_mod8_step2.R",local = TRUE)[["value"]])

     ) # End tabsetPanel
    ) # End Wellpanel
   )))# END Module 8

#          
#          # Module 9
#          tabPanel("Module 9",UImodule9()) # END Module 9
     
#          # Module 10
#          tabPanel("Module 10",module10()) # END Module 10
     
)