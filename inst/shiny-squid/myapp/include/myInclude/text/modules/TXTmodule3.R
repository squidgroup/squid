
module3_txt <- list(
  "title"         = "Module 3: Exploring the impact of sampling "
)

# Step 1 --------------
Mod3Step1_txt <- list(
  
  "title"      = "Step 1: Sampling and non-random environment",
  "subgoal"    = "<b>Sub-goal:</b> to explore how hidden patterns in environment combined with variance in sampling 
                  affect estimates of variance parameters and their interpretation. ",
  "intro"      = paste("<b>Introduction:</b> In modules 1 & 2, we developed a partitioning of phenotypic variance 
                  into several components (the variance among individuals, $V_",NOT$devI,"$, the variance 
                  caused by measurement error, $V_",NOT$error,"$, and variance caused by the environment, $V_{",NOT$env,"}$). 
                  In the final step of Module 1, we illustrated how measurement of the environment 
                  could help explain some of the variance. Often, when we study phenotypes in 
                  natural populations, many aspects of the environment that could affect phenotypes
                  will be unknown and so not measured. In Step 3, this unmeasured environmental 
                  variance ended up as &ldquo;residual&rdquo; variance, and it had no effect on the estimate 
                  of among-individual variance because the environment was randomly determined from 
                  one sampling period to another and all individuals experienced the same environment. 
                  In module 3, we explore what happens when we address this obviously simplified assumption. 
                  For example, suppose the environment changes steadily over the sampling period. 
                  What happens when the pattern of how an investigator measures individuals varies, 
                  such as if the timing of measurement is different for different individuals?", sep=""),
  "exercise"   = paste("<b>Exercise:</b> As in previous simulations, we will generate a new group of 
                  individuals, with phenotypic variance caused by measurement error ($V_",NOT$error,"$), 
                  individual differences ($V_",NOT$devI,"$), and the impact of the environment ($V_{",NOT$env,"}$).", sep=""),
  "para1"      = paste("As before, you can set $V_",NOT$error,"$, $V_",NOT$devI,"$ and $V_",NOT$env,"$, but remember that 
                  these must add up to 1 and for this module, $V_",NOT$env,"$ must be greater than 0.", sep=""),
  "para2"      = "The environment for this simulation is, for convenience, set as being linear over time, 
                  affecting all individuals similarly (i.e., it is &ldquo;shared&rdquo;), having an intercept 
                  effect on phenotype of 0, and a slope that you can input (we recommend at first 
                  that the slope be >> 0).",
  "para3"      = "You also must enter parameters for variance in the sampling of timing within and among individuals. 
                  For this simulation, the number of expressions of the phenotype will be set by us at 100. 
                  While you can vary the number of measurements taken, for this module to effectively 
                  illustrate the issues with sampling, the number of measurements must be much less than 100. 
                  The key parameter to be entered by you will be the among-individual variance in timing 
                  of those records. To illustrate, below are examples of sampling records for a small number 
                  of individuals when the among-individual variance in sampling of timing is 0.1, and when it is 0.9."  ,
  "para4"      = "Now you can input your own values.",
  "para5"      = "Immediately show graphical output of subset of individuals illustrating sampling for the values user entered.",
  "results"    = "<b>Results</b>",
  "para6"      = "The model we assume to be true (but which is not since the environmental effect is unknown) is:",
  "para7"      = "A mixed statistical model estimates the parameters:",
  "para8"      = paste("The above should show that if the unmeasured environment changes over time AND there is 
                  among-individual variance in sampling, then some of the unknown V<sub>ES</sub>  is placed into 
                  residual variance (making it larger than the true $V_",NOT$error,"$), and some ends up in the 
                  estimated $V_",NOT$devI,"$, also making it bigger than it should be.",sep=""),
  "conclusion" = "<b>Conclusion</b>",
  "para9"      = paste("This exercise demonstrates that if there is any variance in timing of sampling, then estimates 
                  of $V_",NOT$devI,"$ will be incorrect since inevitably there are systematic differences 
                  in environments over time. Sampling biases thus can produce &ldquo;pseudo-personality&rdquo; 
                  or &ldquo;pseudo-repeatability&rdquo;  (see also refs) and could mislead a researcher 
                  into believing there are consistent differences between individuals when 
                  there are none (or they are much smaller than it appears). 
                  Because among-individual variance in sampling and systematic changes in environment 
                  are extremely likely in real systems, how can we get accurate estimates of $V_",NOT$devI,"$?
                  ", sep=""),
  "para10"     = paste("Because among-individual variance in sampling and systematic changes in environment 
                  are extremely likely in real systems, how can we get accurate estimates of $V_",NOT$error,"$?",sep=""),
  "para11"     = "We explore two solutions to this problem:
                  <ol>
                    <li>Adjust sampling regime to minimize it (go to Step 2)</li>
                    <li>Use mean-centering to account for the problem (go to Step 3).</li>
                  </ol>"
  
)

# Step 2 --------------
Mod3Step2_txt <- list(    
  "title"      = "Step 2: Sampling to reduce effects of non-random environment",
  "subgoal"    = "<b>Sub-goal:</b> Using simulations to generate sampling regimes 
                  that limit the effects of non-random environments.",
  "intro"      = "<b>Introduction:</b> Step 1 revealed a problem—non-random environments through time 
                  and variability in the timing of sampling can create biases in estimates of 
                  among-individual variation. In this step we encourage you to adjust the sampling 
                  regime to minimize this problem. It should be obvious that if all individuals 
                  are sampled with the same timing, then the bias in the estimates of among-individual 
                  variance disappears, but it is worthwhile assessing how close one has to be to 
                  identical sampling and whether there are biases in other parameters that remain. 
                  So, in this step we will allow you to simulate several types of non-random environments 
                  and adjust the sampling regime.",
  "exercise"   = "<b>Exercise:</b> As in Step 1, we will generate a new group of individuals, with phenotypic 
                  variance caused by measurement error (V<sub>ME</sub>), individual differences (V<sub>I</sub>), and the 
                  impact of the environment (V<sub>E</sub>). ",
  "para1"      =  "You now get to set the environment. In Step 1 of this module, we used an environment 
                  that was experienced similarly by all individuals (&ldquo;shared&rdquo;) and which changed 
                  systematically over time. Below, you can change these settings to have environments 
                  that each individual experiences uniquely (&ldquo;unshared&rdquo;), and which changes over 
                  time as some other function (randomly or as a regressive autocorrelated decay function).",
  "para2"      =  "As in step 1, you also must enter parameters for variance in the sampling timing 
                  within and among individuals. As before, the number of expressions of the 
                  phenotype will be set by us at 100, so keep this in mind as you enter values here.",
  "para3"      =  "Immediately show graphical output of subset of individuals illustrating sampling 
                  for the values user entered.",
  "results"    = "<b>Results</b>",      
  "para4"      =  "As before, the model we assume to be true (but which is not since the environmental 
                  effect is unknown) is:",
  "para5"      =  "A mixed statistical model estimates the parameters which we can compare with the true values:",
  "conclusion" = "<b>Conclusion</b>",
  "para6"      =  "The results of any given simulation may vary, but the overall picture that emerges 
                  if you do several simulations should be that when V<sub>E</sub> is small, as you measure each 
                  individual more often and do so more similarly among individuals, the better 
                  your estimates.", 
  "para7"      =  "Did you simulate a population where the environment is not shared among individuals? 
                  If not, try it now. What you should find is that no matter what the sampling regime, y
                  our estimate of VI is too high. To understand, let’s return to the definitions of 
                  the variance components. We defined V<sub>I</sub> as the variance among individuals 
                  that permanently affected their phenotype throughout the sampling period. Biologically, 
                  this can be ascribed to genetic differences or environments acting during development 
                  (e.g., before measurements started). When environments are unshared during sampling, 
                  the environment is affecting the phenotype anew each time it is expressed, but because 
                  the environment is autocorrelated and differs between individuals, apparent individual 
                  differences arise because individuals are in different environments, not because they 
                  entered the period of phenotypic expression differing in their phenotype (note: You may 
                  be thinking that since individuals in the real world choose their environment, then their 
                  phenotype is not solely due to the environment. That is true, but does not change the fact 
                  that for the focal trait, it is sensitive to the environment the individual is in each time 
                  it is expressed. We will get to the issue of multiple phenotypic characters and how they might 
                  integrate in Module 4). ",
    "para8"     =  "Thus, if you do not know what environments are affecting trait expression, sampling in 
                    parallel for all individuals is a possible solution to potential biases created by 
                    non-random environments. But, because unshared environments can create biases even 
                    with identical sampling (and often identical sampling will be nearly impossible to achieve), 
                    the only other solution is to measure the environment and account for possible biases explicitly. 
                    This is explored next in Step 3."
)

# Step 3 --------------
Mod3Step3_txt <- list(  
  "title"      = "Step 3: Measured environments and mean-centering",
  "subgoal"    = "<b>Sub-goal:</b> Illustrating the use of mean-centered data to control for environmental biases",
  
  "intro"      = "<b>Introduction:</b> Step 1 of this module illustrated that environmental effects on 
                  phenotypes can produce biases in estimates of among-individual variance (V<sub>I</sub>).  
                  Step 2 explored how altering sampling regimes could reduce this problem, but also 
                  revealed that in some circumstances no sampling regime would work. Sometimes 
                  individuals experience different environments, and no sampling regime can adjust for that. 
                  However, if investigators can measure the environment, then such differences could be accounted for.",
  "para1"      =  "A central message of the SQuID project is that phenotypic variance exists in hierarchical 
                  levels&mdash;so far we have focused on two, within-individual and among-individual variance. 
                  Our focus has also been on variance in a trait, but the problem we have encountered is 
                  that there can exist within-individual and among-individual variance in environment as well. 
                  If the environment influences phenotype, then an essential question is whether the pattern 
                  in phenotype merely reflects the pattern in environment, or are there multiple causes. ",
  "exercise"   = "<b>Exercise:</b> This exercise follows the same structure as all of our other simulations so far. 
                  We will generate a group of individuals, with phenotypic variance caused by 
                  measurement error (V<sub>ME</sub>), individual differences (V<sub>I</sub>), and the impact of 
                  the environment (V<sub>E</sub>). So, first set the true values of these variances:",
  "para2"      = "The environment can be chosen as in Step 2. It, combined with the sampling regime, 
                  will affect within- and among-individual variance in the environment. ",
  "para3"      = "An interesting consequence of having variance in the environment that 
                  exists both within and among individuals is that there could be either identical 
                  or different effects of that environment on phenotype. For the simulation, 
                  you can choose this, with the default being that they are the same 
                  (thus half the V<sub>E</sub> you inputted above would be within and half among).",
  "results"    = "<b>Results:</b> In module 1, Step 4, we said the statistical model was",  
  "para4"      = "This assumed the environment and the sampling regime was the same for all individuals. 
                  For the current scenario, we need to expand this model as follows:",
  "para5"      = "This splits the environmental variance (V<sub>E</sub>) into two components, one for differences 
                  between individuals in mean environment (aaaaaaaaaaaa),
                  and differences in measured expressions in environment (aaaaaaaaaaa).",
  "para6"      = "A mixed statistical model estimates the parameters which we can compare with the true values:",
  "para7"      = "To fully understand the effects of mean centering, run several simulations using different
                  set&ndash;ups for the environment, sampling regimes, and 1 and 2 values. In particular, run 
                  a simulation with the environment as unshared and having a mild level of non&ndash;randomness 
                  (either linear or decay) from one time step to another.",
  "conclusion" = "<b>Conclusion</b>",
  "para8"      = "Mean-centering within and among individuals is a powerful way to understand the effect 
                  of environment on phenotypes and to control for biases due to environmental effects. 
                  It does, of course, require measures of the environment, ideally those aspects of 
                  the environment and on a timescale that corresponds to how the trait is influenced 
                  by the environment. In some cases mean-centering is not appropriate. For example, 
                  when measuring the effects of prior exposure to a condition, it might be best to 
                  have the intercept of your model be at 0 prior exposures, which would not be the 
                  mean number of prior exposures. We emphasize that to use this technique effectively, 
                  careful thought must be given to what will be measured and when.",
  "finalcaveat" = "<b>A final caveat</b>",
  "para9"      = "Mean-centering is not perfect. It works best when environments are approximately 
                  normally distributed and the effects of environments are approximately linear. 
                  As the SQuID project grows, we may add modules that explore more complex effects 
                  of environment. In doing so we will hopefully have advice on what to do when 
                  mean-centering is not such a good solution. For now, using it is probably better 
                  than not using it, but it is worthwhile to explicitly consider how it might be misleading."
)

