# UI: Module 2 Step 1
span(
    
  #### Title ####
  h4("Step 1: The biology that produces binary, proportional, and count data"),
  
  #### Subgoal ####
  p(HTML("<b>Sub-goal:</b> Introducing how biology may result in non-normally distributed data.")),    
  
  #### Introduction ####
  p(HTML("Introduction: Before you start analyzing your data, one thing you want to consider is the biology of 
         what you're looking at and the how the phenomenon you're interested in is expressed by organisms. 
         Most of the statistical methods we're familiar with make assumptions about how data are distributed 
         (or, really, how errors are distributed), specifically they assume that our data are normally distributed 
         (i.e., having a Gaussian error structure). However, is this always an appropriate assumption?")),

  p("While the assumption of normality (in residuals) may work in many instances, 
    it is also easy to think of biological traits that clearly don't fit this framework. 
    For example, the sex-ratio of your offspring will not be something that can be measured 
    in such a way that the data would be normally distributed and thus can't be analyzed with typical models. 
    This is most apparent in species with temperature-dependent sex determination where the temperature 
    an egg is incubated at determines whether it is female or male :"),
  
  
  p(HTML('<img src="pictures/Alligator_mississippiensis.jpg" alt="Alligator mississippiensis">')),
  p(HTML("<a href='https://en.wikipedia.org/wiki/American_alligator'><i>Alligator mississippiensis</i></a>, 
           a species that exhibits temperature-dependent sex determination.")),
  
  sliderInput("Mod2Step1_temperature",
             "Temperature ($^\\circ C$):",
             value = 28,
             min   = 28,
             max   = 35,
             step  = 0.5,
             width = "500px"
  ),
  
  # Figure: sex-determination proportion
  plotOutput("Mod2Step1_plot_alligator", width = "350px", height = "300px"), 
  
  p("As you can see from playing with the temperature at which a clutch 
    of American alligators is incubated, sex ratio does not conform to 
    a normal distribution and so using something like a regression to understand 
    how temperature affects sex ratio would be inappropriate."),
  
  p("A simpler case than this one is one where sex is determined via particular 
    combinations of allosomes  (i.e. sex chromosomes). In such cases the sex of offspring 
    will be determined via what's known as a Bernoulli process. As an example, 
    let's consider how the coin-flip process of genetic sex determination interacts with 
    the number of coin-flips to give a distribution of sexes:"),
  
  sliderInput("Mod2Step1_n_offspring",
              "Number of offspring:",
              value = 10,
              min   = 2,
              max   = 100,
              step  = 1,
              width = "500px"
  ),
  
  # Simulation run button
  actionButton("Mod2Step1_Refresh_1", label = Modules_VAR$Refresh$label, icon= Modules_VAR$Refresh$icon, class="refreshButton"),
  sim_msg(),
  
  
  # Figure: sex-determination proportion
  plotOutput("Mod2Step1_plot_coin_flip", width = "350px", height = "300px"), 
  
  p("The outcome of a single coin flip, like that for a single egg, represents what is known as binary data, i.e., 
    this data takes the form of 1 or 0 (A or B, female or male, etc.). Many other biological processes take a similar form: 
    at a particular instance one of two outcomes is possible. Importantly, this process also works when the odds of either 
    outcome are not 50/50:"),
  
  
  sliderInput("Mod2Step1_female_probability",
              "Female probability:",
              value = 0.5,
              min   = 0,
              max   = 1,
              step  = 0.1,
              width = "500px"
  ),
  
  # Simulation run button
  actionButton("Mod2Step1_Refresh_2", label = Modules_VAR$Refresh$label, icon= Modules_VAR$Refresh$icon, class="refreshButton"),
  sim_msg(),
  
  # Figure: sex-determination proportion
  plotOutput("Mod2Step1_plot_female_prob", width = "350px", height = "300px"),
  
  p("This situation is now starting to approach what we saw with temperature-dependent sex determination: 
    the trait we're measuring is always one of two conditions (e.g. female or male) but the probability of either 
    condition is not 0.5 and, in fact, is dependent on some other environmental condition."),
  
  p("Many biological traits are expressed in binary terms. This includes traits expressed repeatedly over one's lifetime, e.g., 
    whether a bird mates during a particular breeding season or whether a plant flowers during a particular year. 
    As you'll see in later steps of this module, this Bernoulli process can be statistically modelled using what 
    is known as a binomial distribution restricted to binary outcomes. For now, let's consider some other types of 
    distributions and generating patterns we might see."),
  
  
  h5("Proportional Data"),
  
  p("What if instead of a single egg, we consider the sexes of a clutch of animals? 
    In this case the sex of each individual egg is the product of a Bernoulli process but 
    the data we might collect is actually in proportional form, e.g., proportion of males in the clutch. 
    In fact, we actually were plotting data in this sort of manner in all three of our figures above. 
    In this case what matters is the probability of either outcome and, as you saw above, 
    the number of coin flips (or eggs) and the probability of either of the two outcomes."),
  
  
  p(HTML("<u>Exercise:</u> explore the effects of the number of coin flips/eggs/trials and the probability.")),
  
  
  sliderInput("Mod2Step1_n_offspring_2",
              "Number of offspring:",
              value = 50,
              min   = 2,
              max   = 100,
              step  = 1,
              width = "500px"
  ),
  sliderInput("Mod2Step1_female_probability_2",
              "Female probability:",
              value = 0.5,
              min   = 0,
              max   = 1,
              step  = 0.1,
              width = "500px"
  ),
  
  # Figure: histogram of female proportions
  plotOutput("Mod2Step1_plot_female_hist", width = "350px", height = "300px"),
  
  p(HTML("Many types of biological data might similarly be expressed as proportions: 
    for example, the proportion of seeds that successfully germinate and the proportion of females in a group. 
    This type of proportional data is <b>binomially distributed</b>.")),
  
  p(HTML("<i>When proportional data are not from a binomial distribution</i>")),
  
  p("The important characteristic of the data types above is that they are generated via a Bernoulli process: 
    an egg is either female or male, a seed either germinates or it doesn't. 
    These outcomes are, generally, independent of each other and this is actually the key assumption for 
    analyzing proportional data. Importantly, data that is expressed or summarized as a proportion but 
    is not generated from a Bernoulli cannot be analyzed in the same manner. 
    For example, we might often express time spent performing a particular action as a proportion 
    but how we divide the time increments over which we record are typically not independent of each other. 
    For example, if an individual is currently foraging, it is likely still foraging 
    a second later but way less likely a week later. This distinction can be very important 
    in analyses and when data not generated by a Bernoulli processes are analyzed as a proportion, 
    any resulting p-values and confidence estimates will be inappropriate."),
  
  
  h5("Count Data"),
  
  
  p("Besides binary proportional data, another major class of data is count data. 
    Examples include traits like how many clutches a bird has in a single year and how many 
    flowers a plant produces during a growing season. In both of these examples the key aspect 
    is that some act is performed or something is produced additively . The average production 
    over some sampling period (breeding season, life-time, etc.) is then the average of what is 
    known as a Poisson distribution, i.e. the rate ($\\lambda$) of the Poisson process as discussed 
    in the next step of this module. This mean then determines the shape of resulting distribution:"),
  
  
  sliderInput("Mod2Step1_poisson_rate",
              "Mean ($\\lambda$) of the Poisson distribution:",
              value = 1,
              min   = 1,
              max   = 20,
              step  = 1,
              width = "500px"
  ),
  
  # Figure: histogram of counts
  plotOutput("Mod2Step1_plot_count_hist", width = "350px", height = "300px"),
  
  p("As you may have found above, if you set a high enough mean, the Poisson distribution begins 
    to visually resemble a normal distribution (if you didn’t do so earlier enter a large number for the above figure). 
    Importantly, however, this distribution is not actually normal and should not be considered 
    as one because Poisson distributions have the additional property that the variance 
    of the distribution is equal to its mean."),
  
  div(class="line"),
  
  actionLink("Mod2Step1GotoStep2", label = "Next Step (2) >>", class= "linkToModuleSteps") # Go to next step
)