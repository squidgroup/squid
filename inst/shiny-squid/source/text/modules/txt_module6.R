
module6_txt <- list(
  "title"         = "Random-slope regression",
  "goal"          = "<b>Goal:</b> To explore situations where individuals may not only differ 
                    in their intercepts but also in the slope of their reaction to changes 
                    in a given environmental variable. Random slope regression is a great 
                    tool to analyse among-individual differences in phenotypic plasticity."
)

# Step 1 --------------
Mod6Step1_txt <- list(
  
  "title"      = "Step 1: Introduction to random slopes",
  "subgoal"    = "<b>Sub-goal:</b> to develop understanding of hierarchies in variance when individuals 
                  express their traits as a response to a changing environmental factor, 
                  but do so differently. In other words, 
                  individuals have variable reaction norms in both intercept and slope.",
  
  "intro"      = paste0("<b>Introduction:</b> Modules &ldquo;<i>",Module_titles$mod1,"</i>&rdquo; and &ldquo;<i>",Module_titles$mod3,"</i>&rdquo; 
                        have explored how among-individual $(V_",NOT$devI,")$ variance in mean values can be modelled. 
                        In Step 3 of module &ldquo;<i>",Module_titles$mod1,"</i>&rdquo;, individuals also varied, to some extent, 
                        in how an environmental factor influenced the expression of their phenotypes around this mean, 
                        and these deviations produce within-individual variance as a response 
                        to environmental fluctuations $(V_{",NOT$mean,"})$. Throughout all these lessons, 
                        we have assumed that if the environment changes, individuals change their phenotype 
                        in exactly the same way. This is a dubious assumption and in most biological situations, 
                        it is more reasonable and often more interesting to account for the fact that individuals 
                        might vary in how the environment affects phenotype. For example, individuals may differ 
                        in how they cope with a stressful situation, and less tolerant individuals will alter their 
                        activity to a greater degree in the presence of a stress factor (e.g. high temperature, 
                        presence of a predator, or shortage of food) than stress-tolerant conspecifics. In general, 
                        among-individual differences in plasticity (also called individual by environment interaction; $I\\times E$) 
                        are necessary if adaptive plasticity is to be under selection, and insofar as such differences have 
                        a genetic basis ($G\\times E$), such interactions are necessary for the evolution of plasticity. 
                        Studies of $I\\times E$ in natural populations remain relatively rare. 
                        Here we introduce random-slope regression, in which the assumption about a common within-individual 
                        variance (and plasticity) is relaxed and we explicitly estimate among-individual variance in slope."),
 
  "exercise"  = paste0("<b>Exercise:</b> Here we depart from the random-intercept regression model presented in Step 4 of module 
                       &ldquo;<i>",Module_titles$mod1,"</i>&rdquo; by introducing a parameter that allows defining individual 
                       deviations from the population mean response to environmental changes. 
                       The fundamental difference between the random-intercept regression and the random-slope model, 
                       is that while the former uses individual-level &ldquo;random&rdquo; effects to model 
                       individual-specific intercepts (i.e. that depict the among-individual variance of mean 
                       trait values), the latter introduces an additional &ldquo;random&ldquo; effect on the slopes 
                       (i.e. that describe how individuals respond to changing environments). 
                       For the random-intercept regression, we used the following model to recreate 
                       our simulated set of effects:"),
  "RCode1"    = "# install.packages(&quot;lme4&quot;)<br>
                 LMM1 <- lme4::lmer(Phenotype ~ 1 + X1 + (1|Individual), data = sampled_data)",
  "para1"     = "We will modify this model as:",
  "RCode2"    = "LMM2 <- lme4::lmer(Phenotype ~ 1 + X1 + (1|Individual) + (0 + X1|Individual), data = sampled_data)",
  "para2"    = paste0("in which $",NOT$devS,"_",NOT$ind,"$ is the effect of measured environment 
                $",NOT$env,"_{",NOT$time, NOT$ind,"}$
                on the measure of phenotype in the <i>i <sup>th</sup></i> individual. 
                Accordingly, we can apply the following parameterization regimes:"),
  "note"     = "Note: you can consider residual variance as solely measurement error variance, 
                or you could consider that it might also include some unknown environmental 
                variance in addition to the known environmental variance determined below.",
  "para3"    = "Set in the background is the environmental variable and its sampling. 
                Here we should use uniform sampling, where each individual is sampled 
                at the same time so there are no biases. By definition, 
                our environmental variable is also mean centered.",
  
  "results"  = "<b>Results:</b> A mixed-effects statistical model estimates these parameters:",
  "para4"    = "Individual-specific responses can be best visualised by plotting 
                the individual-specific regression lines.",
  
  "point"   = "<b>Point:</b> Again, we performed linear regression approach in a mixed model framework, 
                but in this case we generated multiple regression lines 
                (by defining different intercepts and slopes) for each individual in a single model 
                to describe the individual-specific effect of an explanatory variable. 
                Individual-specific responses to changes that occur along an environmental 
                gradient form &ldquo;reaction norms&rdquo;. These reaction norms, if the environmental 
                gradient is centred around zero, can be characterized by their intercept that 
                describes individual mean expression values, and by their slope that expresses 
                the plasticity of traits within individuals. Statistically, one can evaluate whether 
                the random-intercept or the random-slope model fits the data at hand better. 
                However, the consideration of differences in how individuals respond to environmental 
                fluctuations may be straightforward on a biological basis. This model simultaneously 
                accommodates tests for individual personality differences (i.e. the calculation of 
                repeatability makes sense) as well as tests for individual by environment interaction.",
  "RCode"    = "# install.packages(&quot;lme4&quot;)<br>
               LMM <- lme4::lmer(Phenotype ~ 1 + X1 + (1|Individual) + (0 + X1|Individual), data = sampled_data)"
)

# Step 2 --------------
Mod6Step2_txt <- list(    
  "title"      = "Step 2: Intercept-slope correlation",
  "subgoal"    = "<b>Sub-goal:</b> To introduce and understand correlation (covariance) between intercepts and slopes.",
  "intro"      = "<b>Introduction:</b> Up until this point, we have been making the hidden and simplifying 
                  assumption that each variable in our variance partitioning is independent of other variables. 
                  In other words, we have assumed, for example, that measurement error is not correlated with 
                  individual intercept or environment. For the most part, we will continue to make these assumptions, 
                  but the introduction of random slopes means we have two variances, in intercepts and slopes, 
                  that are part of phenotypic variance and attributes of individuals. Biologically, 
                  it becomes very interesting if there is a correlation between intercepts and slopes. 
                  That correlation can be either positive or negative, and we will have you simulate data 
                  both ways so you can see what the sign of the correlation does to the pattern of variance 
                  within and among individuals.",
  
  "para1"      = "We use the same equation as before, e.g.,",
  "para2"      = "but now we make the formally hidden assumption explicit with some new nomenclature:",

  "para3"      = paste0("This means that the intercept value of the <i>i <sup>th</sup></i> individual $(",NOT$devI,"_",NOT$ind,")$ 
                        and the slope of that individual $(",NOT$devS,"_",NOT$ind,")$ are distributed as multivariate 
                        normal with means for each attribute of 0 and a variance-covariance structure 
                        of $\\Omega_{",NOT$devI,NOT$devS,"}$. We then specify the variance-covariance structure in a 2 by 2 matrix. 
                        You have seen the variance terms ($V_",NOT$devI,"$  and $V_",NOT$devS,"$ ) before, 
                        but the new term introduced here is $Cov_{",NOT$devI, NOT$devS,"}$, which is the measure of 
                        how much $",NOT$devI,"_",NOT$ind,"$ and $",NOT$devS,"_",NOT$ind,"$ covary together."),
  
  "exercise"   = "<b>Exercise:</b> We will do a very similar simulation as in Step 1, 
                  but you will then enter some value for the covariance between the intercept and the slope. 
                  You will enter this covariance as a standardized value by expression it as 
                  a correlation ranging between -1 and +1:",
  "para4"      = "We will then examine the appearance of the resulting data.",
  
  "results"    = "<b>Results:</b> A mixed-effects statistical model estimates these parameters:",
  "note1"      = paste0("Note that we have slightly changed what is presented in the table above. 
                        Whereas earlier we presented the phenotypic variance due to a particular term 
                        (e.g., the variance due to individual differences in response to an environmental 
                        factor, or $V_{",NOT$devS,NOT$env,"}$), above we present what many statistical packages produce, 
                        which in the case of random slope, is the variance in slope $(V_",NOT$devS,")$ 
                        rather than the phenotypic variance due to slope."),
  "para5"      = "The best way to develop an intuition about what the covariance represents 
                  is to compare a graph with and without it. Below are two panels of simulated data. 
                  The one on the left shows your simulated individuals without the covariance, 
                  and the one on the right is the same set with the covariance you entered. 
                  The difference will, of course, be more obvious if you entered a large covariance (correlation).",
  
  "para6"      = "Try this step a couple of times to explore in particular what a negative 
                  versus positive covariance does to the pattern of reaction norms.",
  
  "point"      = "<b>Point:</b> Intercept-slope covariance is an additional layer 
                  to understanding the phenotypic equation. You will note that it 
                  is not a parameter in the equation itself, but is rather describing 
                  the relationship between two parameters in the equation—that is, 
                  how they behave across individuals. 
                  The covariance does normally appear in a partitioning of variance:",
  "note2"      = paste0("Note: In SQuID $E(",NOT$env,")=0$ and hence the covariance does not contribute to total phenotypic variance."),
  "para7"      = "The biology of the covariance is just beginning to be explored 
                  (e.g., <a href='http://onlinelibrary.wiley.com/doi/10.1111/j.1461-0248.2011.01698.x/abstract' target='_blank'>Mathot et al. 2011</a>).
                  Undoubtedly further understanding of it will contribute to understanding 
                  the evolution of plasticity, since the covariance links variation 
                  in plasticity with variation in mean trait values, 
                  and therefore could affect evolution in unexpected ways.",
  "RCode"      = "# install.packages(&quot;lme4&quot;)<br>
                 LMM <- lme4::lmer(Phenotype ~ 1 + X1 + (X1|Individual), data = sampled_data)"
)

# Step 3 --------------
Mod6Step3_txt <- list(  
  "title"      = "Step 3: Sample size and precision",
  "subgoal"    = paste0("<b>Sub-goal:</b> to develop understanding of how the level of replication 
                        in terms of the number of individuals and samples per individual 
                        influence the estimation of the among-individual variance in intercepts $(V_",NOT$devI,")$, 
                        slopes $(V_{",NOT$devS,"})$ and their correlation $(Cor_{",NOT$devI,",",NOT$devS,"})$."),
  "intro"      = "<b>Introduction:</b> Several papers have addressed the issue of sample size requirements 
                  for random regression models. The general conclusion is that accurate estimates 
                  of random regression parameters require relatively high sample sizes, 
                  particularly for estimating the correlation between intercepts and slopes. 
                  When planning an optimal experimental design, researchers are faced with a 
                  trade-off between the number of individuals and the number of observations 
                  per individual they can sample.",
  "exercise"   = "<b>Exercise:</b> We will perform a set of simulations that will address 
                  what is the sampling design, in terms of number of individuals and repeats 
                  per individual, that will maximize the precision of parameter estimates. 
                  Let's assume that some researchers want to explore the effect of sample 
                  size in their estimates. They also want to determine the best decision 
                  in terms of allocating effort in sampling more individuals or more repeats 
                  per individual. The use of SQuID helps researchers to determine the best 
                  sampling design. They will use the same equation as we used in the step 2, e.g.,",
  "RCode"      = "# install.packages(&quot;lme4&quot;)<br>
                 LMM <- lme4::lmer(Phenotype ~ 1 + X1 + (X1|Individual), data = sampled_data)",
  "table"      = "The table below summarises the value of each parameter of the model:",
  "para1"      = paste0("For these simulations we are going to use a stochastic environment effect $(",NOT$env,")$. 
                  We will also sample individual trait expressions the same number of times but 
                  at different instances of time among individuals. The among-individual variance 
                  in sampling of timing will be 0."),
  "para2"     = "You should base your decision on the graphic representation of the estimates. 
                 The figure below is a summary of parameter estimates of 100 models performed 
                 from the 100 simulated datasets.",
  "para3"     = "For each sample size there will be three combinations: one with more individuals than repeats, 
                one with equal number of repeats and observations, and one with more repeats than individuals.",
  "para4"     = "Each histogram shows the frequency distribution of each parameter based on the 100 simulations 
                that you ran for each of the three study designs. &ldquo;NI&rdquo; is the number of individuals and 
                &ldquo;NR&rdquo; is the number of repeats per individuals. The red line represents the &ldquo;true&rdquo; 
                value that you set for this parameter above. The histograms provide information 
                on the expected bias (inaccuracy) and imprecision that comes with each chosen study design. 
                If the chosen study design would be perfect (i.e., resulting in highly precise and accurate estimates) 
                your parameter estimates should be narrowly distributed around the true value. 
                If your estimates are precise but inaccurate, the distribution should be narrow 
                but peaking away from the true value. If your estimates are accurate but imprecise 
                the distribution should be peaking at the true value but simultaneously very broad. 
                Finally, if your estimates are imprecise and inaccurate the distribution would both 
                wide and peaking away from the true value."
)