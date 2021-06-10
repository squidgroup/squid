# UI: Module 2 Step 3
span(
    
  #### Title ####
  h4("Step 3:  Introduction to generalized linear mixed-effect models (GLMMs)"),
  
  #### Subgoal ####
  p(HTML("<b>Sub-goal:</b> to understand the idea of the link and inverse link function and overdispersion 
         and how (dis)similar it is to model Gaussian and non-Gaussian traits.")), 
  
  #### Introduction 1 ####
  p(HTML("<b>Introduction 1:</b> In other modules, we assumed the trait of interest, $y$ follows a Gaussian distribution 
         and we used a mixed-effects modeling framework to partition variance in $y$. 
         For a non-Gaussian trait, $y$, somehow we would like to make  $y$ normally distributed , 
         removing the mean-variance relationship. Then, we could use the same mixed-effects model framework 
         we have been using in other modules so far. This 'transformation' can be done using the 'link' function 
         in generalized linear models (GLMs) if y values are independent of each other or in 
         generalized linear mixed models (GLMMs) when y values are repeated measures or have a correlational structure. 
         Also, the 'inverse link' function can bring results from the 'link' scale ($y'$) back to the original non-Gaussian data scale. 
         This link scale is often called as the latent scale. For example, a commonly used link function for binary 
         (and proportional) data is the logit link function. The logit function and its inverse can be written as:")),
  
  
  span("$$y \\sim Bernoulli(p)$$"),
  span("$$logit(p) = ln(\\frac{p}{1-p}) = y'$$"),
  span("$$logit^{-1}(y') = \\frac{1}{1 + e^{-y'}} = p$$"),
  
  p("Unlike $y$, $y'$ is assumed to follow a Gaussian distribution."),
  
  p(HTML("<b>Exercise 1:</b> To understand the concept of the link and inverse link function better, 
         we will simulate $y'$ (a non-Gaussian trait on the latent scale) and convert simulated values 
         first to $p$ and the non-Gaussian trait $y$.")),
  
  displayRCode("# number of values generated.</br>
                n <- 10000</br></br>
                # Use the Gaussian distribution to generate n values</br> 
                # with a mean of 0 and a standard-deviation of 1.</br>
                y_prime <- rnorm(n, 0, 1)</br></br>
                # Convert these values to probabilities</br> 
                # using the inverse-logit function.</br>
                p <- plogis(y_prime)</br></br>
                # Use the Bernoulli distribution to generate n values</br> 
                # with the probability vector p.</br>
                y <- rbinom(n, size = 1, prob = p)</br></br>
                # Note that we used the binomial function with one trial,</br> 
                # which is equivalent to the Bernoulli distribution."),
  
  p("Note that we can get binary data from a normal distribution by using the inverse link function 
    and Bernoulli distributions. But the reverse operation requires modeling (which we will practice below) 
    because the logit transformation of 0 and 1 is negative and positive infinity, respectively."),
  
  
  #### Introduction 2 ####
  p(HTML("<b>Introduction 2:</b> Another concept you need to learn for modeling non-Gaussian data is overdispersion, 
         which we will explain more below ('dispersion' relates to variability of a distribution like variance but 
         it is a more general term). Imagine that we record the number of female and male offspring that female animals 
         have per breeding season across the number of years ($y$ being a concatenation of female and male offspring per 
         female $i$ and per season $h$. Then, using the logit link function with a binomial error structure, $y$ can be 
         expressed as a GLMM:")),
  
  span("$$y_{hi} \\sim binomial(m_{hi}, p_{hi})$$"),
  span(paste0("$$p_{hi} = logit^{-1}(",EQ3$mean0," + ",EQ1$dev0," + o_{hi})$$")),
  span(paste0("$$",EQ1$dev0," \\sim N(0,V_", NOT$devI,")$$")),
  span("$$o_{hi} \\sim N(0,V_o)$$"),
  

  p(paste0("where logit<sup>-1</sup> is the inverse link function and ohi is overdispersion,
           which is normally distributed as for the individual effect $",EQ1$dev0,"$ (note that we skip the notation $y'$
           which we used above). All the key parameters (population mean $",EQ3$mean0,"$ and individual-specific deviation $",EQ1$dev0,"$)
           are estimated on the latent scale where random effects are (assumed to be) normally distributed.
           It is also interesting to notice that $o_{hi}$ is very much like $",NOT$error,"_{hi}$ (residuals),
           which we have seen in other modules; in the statistical literature, $o$ is known as additive (over)dispersion
           because there is an alternative way of implementing dispersion known as multiplicative overdispersion
           (for more details of additive and multiplicative dispersion, see Nakagawa and Schielzeth 2010).
           The overdispersion $o_{hi}$ is also known as the observation-level random factor (effect)
           because the number of categories of this random effect matches the number of data points,
           this aspect is also somewhat analogous to the residuals in a normal mixed-effects model.")),
  
  p("Overall the GLMM above looks very much like normal mixed-effects models apart from the overdispersion 
    and the (inverse) link function, which connects phi and yhi via binomial distributions. 
    For a binary trait (dead or alive, male or female, present or absent), $m_{hi}$ is always 1. Also $o_{hi}$ 
    is always 0 because overdispersion is not 'identifiable' in binary data."),
  
  p("Now, imagine that the number of matings ($y$) per male ($i$) animal and per season ($h$). 
    Then, using the log link function with a Poisson error structure, $y$ can be written as a GLMM:"),
  
  span("$$y_{hi} \\sim Poisson(\\lambda_{hi})$$"),
  span(paste0("$$\\lambda_{hi} = exp(",EQ3$mean0," + ",EQ1$dev0," + o_{hi})$$")),
  span(paste0("$$",EQ1$dev0," \\sim N(0,V_", NOT$devI,")$$")),
  span("$$o_{hi} \\sim N(0,V_o)$$"),
  
  p("where exp is the inverse link function (of log), and the other notations are the same as above. 
    Again, on the latent scale, random effects are all normally distributed, and the equation is very 
    similar to a Gaussian trait."),
  
  p(HTML(paste0("<b>Exercise 2:</b> We now set the 3 parameters ($",EQ3$mean0,"$, $V_", NOT$devI,"$, and $V_o$) 
                on the latent (link) scale in the Poisson GLMM with the log link function and generate count data. 
                The simulation is done with 100 individuals that are sampled 5 times each."))),

  # population mean effect (beta0)
  getSliderInput("Mod2Step3_B0", Modules_VAR$B0),
  
  # among-individual variance (Vi)
  getSliderInput("Mod2Step3_Vi", Modules_VAR$Vi),
  
  # overdispersion variance (Vo)
  sliderInput("Mod2Step3_Vo",
              "Overdispersion variance ($V_o$):",
              value = 0.1,
              min   = 0,
              max   = 1,
              step  = 0.1,
              width = "500px"
  ),
  
  # Simulation run button
  actionButton("Mod2Step3_Run", label = Modules_VAR$Run$label, icon= Modules_VAR$Run$icon, class="runButton"),
  runningIndicator(),
  sim_msg(),
  
  # Table : display model output
  uiOutput("Mod2Step3_summary_table"),
  
  displayRCode('# Here the phenotype value is at the latent (i.e., Gaussian) scale,</br>
                # we then transform it into the Poisson scale</br>
                sampled_data[ , "Phenotype_pois"] <- rpois(nrow(sampled_data), exp(sampled_data[ , "Phenotype"]))</br>
                </br>
                sampled_data[ , "Overdispersion"] <- as.factor(1:nrow(sampled_data)) # the observation-level random effect</br>
                </br>
                GLMM <- lme4::glmer(Phenotype_pois ~ 1 + (1|Individual) + (1|Overdispersion), family = poisson(link=log), data = sampled_data)'),
  
  p(HTML("<b>Conclusion:</b> The underlying biology that generates phenotypes is important to consider 
         because not all biological processes will produce Gaussian data. Deciding how you are going to 
         analyze your data requires that you consider how it was generated and this biology will determine 
         the types of analyses you conduct. Once we identify non-Gaussian data such as binary, 
         proportional and counts. Then, wee can use GLMMs to model these data (with appropriate link function 
         and error structure, e.g., log link function with a Poisson error structure). On the latent scale, 
         we can estimate regression coefficients and variance components just like how we model Gaussian data.")),
  
  
  p(strong("References:")),
  p(HTML("Nakagawa, S. & Schielzeth, H (2010) Repeatability for Gaussian and non-Gaussian data: a practical guide for biologists. 
          <i>Biological Reviews</i>, 85, 935-956.
          <a href='https://doi.org/10.1111/j.1469-185X.2010.00141.x' target='_blank'>doi: 10.1111/j.1469-185X.2010.00141.x</a>")),
  
  div(class="line"),
  
  actionLink("Mod2Step3GotoStep2", label = "<< Previous Step (2)", class="linkToModuleSteps"), # Go to previous step
  
)