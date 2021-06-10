# UI: Module 2 Step 2
span(
    
  #### Title ####
  h4("Step 2: Introduction to Bernoulli, binomial and Poisson distributions"),
  
  #### Subgoal ####
  p(HTML("<b>Sub-goal:</b> to learn statistical properties of the three key non-Gaussian distributions.")), 
  
  #### Introduction ####
  p(HTML("Introduction: In Step 1, we considered biological scenarios where non-Gaussian data could be produced. 
         The three important types of data are binary, proportional and count data, each of which corresponds to a 
         uniquely named statistical distribution, namely Bernoulli, binomial and Poisson. Remember that a Gaussian 
         (normal) distribution is characterized by a mean ($\\mu$) and a variance ($V$). These two quantities are called 
         statistical parameters for the Gaussian distribution. Statistical parameters for the three non-Gaussian distributions 
         are not the mean and variance (at least they are not called so). Let's look at the statistical parameters for 
         each non-Gaussian distribution now.")),
  
  p(HTML("A <b>Bernoulli</b> distribution is characterized by only one parameter, p, which is often interpreted 
         as a probability of success. Or more biologically, for example, you can see it as the probability 
         of female offspring (see Step 1). More formally, we can write a trait y, which follows a Bernoulli 
         distribution along with its mean ($\\mu$) and variance ($V$) as:")),
  
  span("$$y \\sim Bernoulli(p)$$"),
  span("$$\\mu = p$$"),
  span("$$V = p(1-p)$$"),
  
  p(HTML("A <b>binomial</b> distribution has one more statistical parameter, which is m, the number of trials. 
         A biological example is the number of offspring in a brood (as $m$) with the probability of female being $p$. 
         More formally, we can write a proportional trait, $y$ as:")),
  
  span("$$y \\sim binomial(m,p)$$"),
  span("$$\\mu = mp$$"),
  span("$$V = mp(1-p)$$"),
  
  p("As you can see a Bernoulli distribution is a special case of a binomial distribution with m being 1."),
  
  p(HTML("Like the Bernoulli distribution, the <b>Poisson</b> distribution has only one statistical parameter. 
         This parameter is often called, $\\lambda$ (termed the 'rate' parameter).  
         We can formally write a count trait following a Poisson distribution as:")),
  
  span("$$y \\sim Poisson(\\lambda)$$"),
  span("$$\\mu = \\lambda$$"),
  span("$$V = \\lambda$$"),
  
  p("As you can see, the mean equals the variance in a Poisson distribution. 
    Also, for both Bernoulli and binomial distributions, the mean and variance are also tightly related. 
    In fact, a unique feature of a Gaussian distribution is non-existence of the relationship between the mean and variance. 
    Non-Gaussian distributions, in general, have, what is called, a mean-variance relationship. 
    It may be interesting to note that a binomial distribution becomes 
    a form of a Poisson distribution when p is very small because:"),
  
  span("$$V = mp(1-p) \\approx mp = \\mu$$"),
  
  p(HTML("<b>Exercise:</b> We explore the mean-variance relationship for Bernoulli distributions with p ranging from 0 to 1.")),
  
  # Figure: mean-variance relationship in Bernoulli distribution
  fluidRow(
    column(6,
      plotOutput("Mod2Step2_plot_bernoulli_mean", height="200px")
    ),
    column(6,
      plotOutput("Mod2Step2_plot_bernoulli_var", height="200px")
    )
  ),
  
  p("We now do the same for Poisson distributions where $\\lambda$ varies between 0.5 and 100."),
  
  # Figure: mean-variance relationship in Poisson distribution
  fluidRow(
    column(6,
           plotOutput("Mod2Step2_plot_poisson_mean", height="200px")
    ),
    column(6,
           plotOutput("Mod2Step2_plot_poisson_var", height="200px")
    )
  ),
  
  div(class="line"),
  
  actionLink("Mod2Step2GotoStep1", label = "<< Previous Step (1)", class="linkToModuleSteps"), # Go to previous step
  span(Modules_VAR$StepLink$sep, class="step-Link"),
  actionLink("Mod2Step2GotoStep3", label = "Next Step (3) >>", class="linkToModuleSteps") # Go to next step

)