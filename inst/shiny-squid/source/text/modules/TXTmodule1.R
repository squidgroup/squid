
module1_txt <- list(
  "title"         = paste("Partitioning variances: Differences between $V_",NOT$total,"$, $V_",NOT$devI,"$ and 
                           measurement error variance",sep=""),
  "goal"          = "<b>Goal:</b> to develop understanding of hierarchies in variance when individuals express traits repeatedly.",
  "statModTitle"  = "<b>Statistical model:</b>"
)

# Step 1 --------------
Mod1Step1_txt <- list(
  
  "title"      = "Step 1. Mean and variance when the trait is expressed once and does not change",
  "subgoal"    = "<b>Sub-goal:</b> Illustrating the concept of variance and mean, 
                  for traits with no within-individual variance but which is measured with error.",
  "intro"      = paste("<b>Introduction:</b> This is the simplest possible situation. 
                       A trait as measured for a group of individuals from a population 
                       is characterised by a mean ($",NOT$mu,"$) and a variance ($V_",NOT$total,"$).",sep=""),
  "exercise"   = "<b>Exercise:</b> In this situation we assume that we only have one value of the trait for each individual. 
                  You first have to decide how many individuals to measure.",
  
  "para1"      = paste("In every case that we measure something, we know that we are making an error in the measurement. 
                       This error is assumed to be non-directional and hopefully represents only a small portion of 
                       the total variance $V_",NOT$total,"$. Below play with the error term. 
                       Generally measurement error variance should not be high, 
                       ideally lower than 5% of the total variance, but of course some traits 
                       can be associated with much higher measurement error.",sep=""),

  "explanation1"  = paste("An explanation of notation: There are several kinds of unaccounted variance 
                          in a statistical model. This are called, variously, &ldquo;error variance&rdquo; or &ldquo;residual variance&rdquo;.  
                          Measurement error is one source of this variance, but as you will see below, 
                          not all unaccounted variance is the result of error. 
                          To distinguish these with notation, we will use $V_",NOT$mError,"$ for measurement error 
                          and $V_",NOT$residualUpper,"$ for the more general residual variance. In this particular step, 
                          $V_",NOT$residualUpper,"=V_",NOT$mError,"$. ",sep=""),
  
  "note1"      = paste("Note that in this module, the total phenotypic variance $(V_",NOT$total,")$ is restrained to 1. 
                       This will allow a better understanding of the proportions of the different model variance components.",sep=""),
  
  "point"      = paste("<b>Point:</b> The variance of measures $(V'_",NOT$total,")$ is often higher than the variance 
                       of true individual variance $(V_",NOT$devI,")$ if there is measurement error 
                       (and there will always be some). The estimate of total variance includes 
                       both individual differences $(V_",NOT$devI,")$ (not measured directly) and measurement error $(V_",NOT$mError,")$. 
                       Note also that the estimated $V'_",NOT$total,"$ (total variance of sampled values) 
                       typically differs slightly from 1. This is because we have sampled 
                       from a population where the true total phenotypic variance that includes 
                       individual (biological) variance and measurement error variance, 
                       while equal to 1, may not be 1 in your sample. 
                       The difference thus arises from sampling variance.",sep=""),
  
  "solutions"  = "<b>Solutions:</b> First work to reduce the measurement error; 
                  to do so, you need to know the magnitude of measurement error. 
                  This requires measuring individuals more than once. 
                  This is explained in step 2 of this module.",
  
  "statmodel1"  = "Throughout these modules we will provide you with the statistical 
                  model that we've explored. These come in two forms. 
                  The first is an equation that describes each data point 
                  (particular measurements on individual phenotypes). Since we are using 
                  phenotypic measures as the focus here, we call this the &ldquo;phenotypic equation&rdquo;. 
                  You have just explored the following phenotypic equation:",
  
  "statmodel2"  = paste0("where $",NOT$trait.1,"_{",NOT$time,NOT$ind,"}$ is the measured phenotypic 
                          value of the $h^{th}$ measurement 
                         (in this case $",NOT$time,"$ = 1) on the $i^{th}$ individual, $",NOT$devI,"_",NOT$ind,"$ is the true deviation 
                         of the individual's trait from the population mean 
                         (which is assumed to be 0 for now), and $",NOT$error,"_{",NOT$time,NOT$ind,"}$  is the residual 
                         deviation of that measurement from the true value, 
                         caused in this scenario by measurement error."),
  
  "statmodel3"  = paste0("The second type of equation is a partitioning of variance 
                         where the terms are defined as above. These have direct connection 
                         to the values in the phenotypic equation, with the exception 
                         that in this scenario the variance in $",NOT$error,"_{",NOT$time,NOT$ind,"}$, often called residual variance, 
                         is measurement variance $(V_",NOT$mError,")$")
)

# Step 2 --------------
Mod1Step2_txt <- list(    
  "title"     = "Step 2: Repeatability and measurement error",
  "subgoal"   = paste0("<b>Sub-goal:</b> Learning how to estimate measurement error variance $(V_",NOT$mError,")$ 
                  in traits varying solely among individuals."),
  "intro"     = "<b>Introduction:</b> In the previous step, you estimated the mean and the total 
                  variance of a sample of individuals measured once. 
                  From this you learned that some of the observed 
                  variation in measures might be due to measurement error, 
                  which occurs when the measured value deviates 
                  (for whatever reason) from the biological value. 
                  Here we detail the sampling design that you might 
                  use to directly estimate the magnitude of measurement error; 
                  we do this for a trait where the true value of the trait 
                  is constant for an individual (cf. a &lsquo;fixed&rsquo; rather than &lsquo;labile&rsquo; trait, 
                  e.g. structural size in adulthood).  Measurement error can be estimated 
                  by measuring the same set of individuals multiple times 
                  (preferably in a blind and randomized order). Since their trait values are fixed, 
                  any deviation in measured values across measurements of the same individuals should be due to measurement error.",
  "exercise"    = paste("<b>Exercise:</b> To simulate this new situation, 
                  we advise using the same parameters as in the previous step, 
                  in terms of the number of individuals and measurement error 
                  ($V_",NOT$mError,"$; expressed as a proportion of the total phenotypic variance, $V_",NOT$total,"$) 
                  so that you can compare your output with the previous step:",sep=""),
  
  "para1"   =  "You also need to enter how often each individual is measured. 
                We call this variable &lsquo;Number of trait expressions&rsquo;. 
                For simplicity, we are assuming that each individual 
                is measured the same number of times. Because we are 
                interested in setting up a scenario where individuals are 
                assayed repeatedly, you should set a number equal or greater than two. 
                Play around with how often you measure each individual, 
                because the number of repeated measurements per individual 
                affects how well your estimated values approximate the true values.",
  "para2"   =  "As before, we present the output in the form of a collection 
                of figures and tables that shows you your estimated values 
                (derived from a univariate mixed-effects model) as well as the true values. 
                Compare how the estimated values deviate from the true values; 
                play for example with the &lsquo;number of trait expressions&rsquo; 
                to see how the number of repeated measurements per individual 
                affects your estimates and how much they deviate from the true values.",
  "para3"   =  paste("The three density plots show the distribution of estimated total phenotypic variance $(V'_",NOT$total,")$, 
                the estimated variance among individuals $(V'_",NOT$devI,")$, and 
                the estimated variance within individuals $(V'_",NOT$mError,")$. 
                It demonstrates graphically that $V'_",NOT$total,"=V'_",NOT$devI,"+V'_",NOT$mError,"$.",sep=""),
  
  
  "point"   =  "<b>Point: </b> If you have inputted the same parameter values as for step 1, 
                you see that we are now (more) properly estimating the individual variance. 
                This is because we have directly estimated the within-individual variance 
                due to measurement error by measuring each individual (at least) twice.",

  "para4"   =  "Measurement error can obscure true biological variance. We typically 
                express the amount of biological variance we can measure as a proportion 
                of the total observed variance; this standardized metric 
                is called &quot;repeatability&quot;:",
  "para5"   =  "Repeatability tells you how well your current measurement predicts a 
                future measurement of the same individual. Logically, the repeatability 
                is 1 if there is no measurement error (try it out!). The effect 
                of measurement error can be nicely illustrated in a graph where we plot 
                the first vs. second measurement. ",
  
  "para6"   =  paste0("The scatter plot shows how correlated the first and second measurements are. 
                In the absence of measurement error (i.e., $V_",NOT$mError,"=0$), 
                all points align on a single line ($y=x$). Play around with the settings of $V_",NOT$mError,"$; 
                the higher the value, the more widely the data points get dispersed around $y=x$, 
                i.e. the higher the measurement error the lower our ability 
                to predict an individual's phenotype based on a previous measurement. 
                Repeatability thus provides an assessment of how well one has measured 
                a particular phenotype. Repeatability can be used for several other purposes too, 
                so the way it is calculated may vary some. We will develop some of these nuances later on."),
  "RCode"      = "# install.packages(&quot;lme4&quot;)<br>
                  LMM <- lme4::lmer(Phenotype ~ 0 + (1|Individual), data = sampled_data)"
)

# Step 3 --------------
Mod1Step3_txt <- list(  
  "title"     = "Step 3: Within- and among-individual variance",
  
  "subgoal"   = "<b>Sub-goal:</b> to illustrate hierarchical structure of variance when individuals 
                         express traits multiple times (a trait varies within-individuals).",
  
  
  "intro"     = "<b>Introduction:</b> Individuals express different values of a trait at different 
                times due to the influence of the environment. Collecting two or more measures 
                for each individual provides several types of information. 
                We have already seen that it can allow estimation of measurement error. 
                If the measurements are spread over time, then you can also estimate 
                how much variance is due to the environment and how the phenotype 
                responds to the environment. Finally, repeated measurements can measure 
                whether differences among individuals are solely due to differences in the 
                environment at the time of measurement versus to some other source, 
                such as differences among individuals that were generated before the period of measurement.",
  
  
  "exercise"  = paste0("<b>Exercise:</b> As in the two previous steps we will generate a new group 
                      of individuals but we will also add a new form of variance. 
                      Previously, you generated a population of individuals that varied in 
                      their true value $(V_",NOT$devI,")$ and also due to observer measurement error $(V_",NOT$mError,")$. 
                      We assumed that individuals had the same trait value throughout 
                      the duration of our study. But, what if trait values changed over time, 
                      perhaps in response to some environmental gradient? To start exploring this idea, 
                      let's assume that all individuals would respond to this gradient in the same way, 
                      but they might experience different values of the environment.  
                      This means that in addition to $V_",NOT$devI,"$ and $V_",NOT$mError,"$ there is variation caused by the population 
                      mean response to an environmental effect on the phenotype of an individual. 
                      In this step, the environmental effect is considered as individual-specific 
                      but unknown to the observer. An (individual-) specific environmental effect 
                      means that each individual of the population will experience different environmental 
                      conditions than do other individuals. For instance, the intensity of 
                      intra- and inter-specific competition within a population might be experienced 
                      differently between individuals. An unknown environmental effect represents 
                      environmental values that are not measured thus cannot be included in the statistical analysis. 
                      Earlier we noted that $V_",NOT$residualUpper,"$ is called &ldquo;residual variance&rdquo;. Now, unknown, 
                      environmentally-caused variance will be combined with $V_",NOT$mError,"$ to make $V_",NOT$residualUpper,"$, 
                      and is the primary reason for calling it &ldquo;residual&rdquo; variance.  
                      In reality, there may be many environmental effects, both known and unknown. 
                      We will use the term $V_",NOT$envEffect,"$ to refer to all phenotypic variance caused by the environment. 
                      Known environmental variance will be indicated by $V_{",NOT$mean," ",NOT$env,"}$."),
  
  "exercise2"  = paste0("In this simulation, let's assume there is only one environmental effect. 
                        We will specify it to generate phenotypes, so let's use the term $V_{",NOT$mean," ",NOT$env,"}$. 
                        You can thus try several combinations of $V_",NOT$mError,"$, $V_",NOT$devI,"$, and $V_{",NOT$mean," ",NOT$env,"}$ 
                        (with the constraint that they add up to 1) to uncover how the input values 
                        affect the estimates of repeatability and each variance component. 
                        Just remember that for now $V_{",NOT$mean," ",NOT$env,"}$ is one environmental effect but 
                        it will be measured as part of the residual."),
  
  "para1"    = "Once again you can calculate repeatability as",
  
  "point"    = paste0("<b>Point:</b> We can now estimate a particular variance component 
                of $V_{",NOT$total,"}$ that represents among-individual differences that are consistent through time. 
                This variance is also, in some circumstances, the index for individual 
                &ldquo;personality&rdquo; differences. The residual variance combines both measurement 
                error and the variance caused by the unmeasured (unknown) specific environment to each individual. 
                $V_{",NOT$envEffect,"}$ in general reflects plasticity to unknown environments, 
                with $V_{",NOT$mean," ",NOT$env,"}$ indicating variance due to a specified environmental variable $(",NOT$env,")$, 
                which so far has not been measured."),
  "point2"    = "You can now test whether the number of individuals sampled and the number 
                of repeated measures per individual can affect your estimation.",
  "point3"    = "Repeatability now does not represent the same thing as previously 
                because the denominator of the ratio includes both measurement error 
                and variance due to plasticity. This new repeatability is not simply 
                a measure of your skill at measuring phenotypes. 
                Instead, it now is an estimate of a biological phenomenon: 
                consistent individual differences (i.e. personality in the case of behaviour). 
                This estimate is conservative because measurement error reduces the estimate 
                to be less than the true biological repeatability. Measurement errors 
                in this scenario are not separable from the plastic response of individuals 
                to an unmeasured environment. To calculate measurement error alone, 
                you would have to collect more than one measure on the same trait 
                for each individual in the same environment (e.g. two persons can observe the behaviour 
                of an individual on the same video recording or measure the trait at close to the same time).",
  
  "statmodel"   = "Because the environmental effect in this scenario was unknown, 
                    the statistical models are the same as before, with two exceptions.  
                    First, the phenotypic equation is as before,",
  "statmodel2"  = paste0("except that $",NOT$error,"_{",NOT$time,NOT$ind,"}$ contains both measurement 
                           error and deviations described by $",NOT$mean," ",NOT$env,"_{",NOT$time,NOT$ind,"}$"),
  "RCode"       = "# install.packages(&quot;lme4&quot;)<br>
                  LMM <- lme4::lmer(Phenotype ~ 0 + (1|Individual), data = sampled_data)"
)


# Step 4 --------------
Mod1Step4_txt <- list( 
  "title"     = "Step 4: Explaining Environmental Variance",
  
  "subgoal"   = "<b>Sub-goal:</b> to explain unknown environmental variance.",
  "intro"     = paste0("<b>Introduction:</b> In step 3, we introduced $V_{",NOT$mean," ",NOT$env,"}$ 
                       as the variance caused by the environment. 
                       We did not know what that effect was (it was unmeasured), 
                       but often we can measure the environment and assess its influence on phenotype. 
                       In this step, we demonstrate how that is done."),
 "exercise"    = paste0("<b>Exercise:</b> As before, we will generate a new group of individuals, 
                  with phenotypic variance caused by measurement error $(V_",NOT$mError,")$, individual differences $(V_",NOT$devI,")$, 
                  and specific and measured (i.e. known) environmental effects $(V_{",NOT$mean," ",NOT$env,"})$ on the phenotype of an individual. 
                  As before, you can set $V_",NOT$mError,"$, $V_",NOT$devI,"$ and $V_{",NOT$mean," ",NOT$env,"}$, but we suggest you use the same values you did in Step 3.
                  Remember that these variances must add up to 1."),

  "para1"    = paste0("At this point, we want to expand on the idea of statistical models. 
                The equations that specify effects producing each individual data point can 
                be hypotheses about the real world. In the made-up world of SQuID, 
                these analysis models have the potential of recreating it exactly. 
                The real world is different and most of the lessons we will cover have 
                to do with problems in estimating terms in analysis models when much is unknown. 
                For now, we will specify a model that should recreate our simulated set of effects 
                completely (with the caveat that we are sampling from an infinite population so 
                observed values will differ from input values). This new model will make explicit 
                as well that $",NOT$devI,"_",NOT$ind,"$ is defined as the deviation of each individual from a population value. 
                Until now, that population value has implicitly been 0, so we haven't used it. 
                But, since we are now including a slope term that allows us to explain environmental variation, 
                it is important to also introduce the population intercept term. 
                For now, we will still have the population mean be = 0, but it is appropriate to include 
                this intercept in all equations because it could be some other value. The model is:"),

 "para2"    = paste0("where $",NOT$trait.1,"_{",NOT$time,NOT$ind,"}$ is the phenotype measured at the 
                     $",NOT$time,"^{th}$ time for the $",NOT$ind,"^{th}$ individual, 
                     $",EQ3$mean0,"$ is the population mean phenotype, 
                     $",NOT$devI,"_",NOT$ind,"$ is the individual mean deviation from the population mean for the $",NOT$ind,"^{th}$ individual, 
                     $",NOT$mean,"$ is the population mean effect of measured environment $",NOT$env,"_{",NOT$time,NOT$ind,"}$ on the measure of phenotype, 
                     and $",NOT$error,"_{",NOT$time,NOT$ind,"}$ is the error made in that measurement."),

  "note1"    = paste0("Note that in SQuID each environmental effect $(",NOT$env,")$ is expressed in unit variance 
                      (i.e., $Var(",NOT$env,")=1$) and mean-centered (i.e., $E(",NOT$env,")=0$).
                      Then $V_{",NOT$mean," ",NOT$env,"}=Var(",NOT$mean," ",NOT$env,")=",NOT$mean,"^2Var(",NOT$env,")=",NOT$mean,"^2$"),

  "para3"    = "A mixed statistical model estimates these parameters:",
  "para4"    = paste("Visually, you can see what $",NOT$mean,"$ 
                     is in the following graph:",sep=""),
  "para5"    = "The variance among individuals can still be visualized in this world by plotting 
                each individual&#39;s dataset and line.",
 
  "para6"    = paste0("We encourage you to go back and play around with the magnitude of $V_",NOT$mError,"$ 
                to see how it affects estimates of  $",NOT$mean,"$
                and $V_",NOT$devI,"$. You can also play around with the slope and the ratio of 
                $V_{",NOT$mean," ",NOT$env,"}$ and $V_",NOT$devI,"$ to better understand the effects."),
 
 "point"    = "<b>Point:</b> This exercise introduced explanatory variables (also known as fixed effects). 
               Because individual is a &ldquo;random&rdquo; effect, this is thus a &ldquo;mixed effects&rdquo; model. 
               The fixed effect part is a linear regression. Even if this is all you want to do with your data, 
               it is important to understand that a sampling regime in which individuals 
               are measured more than once creates the need to do linear regression within a mixed model. 
               Although we do not focus much on significance testing here, the structure of data collected 
               in this simulation strongly affects inferences based on hypothesis testing. 
               More importantly, the combination of random effects and fixed effects sets one up 
               to investigate a wide array of processes involved at one or more levels in 
               this hierarchal structure of among versus within-individual variance.",
 "RCode"    = "# install.packages(&quot;lme4&quot;)<br>
               LMM <- lme4::lmer(Phenotype ~ 1 + X1 + (1|Individual), data = sampled_data)"
)