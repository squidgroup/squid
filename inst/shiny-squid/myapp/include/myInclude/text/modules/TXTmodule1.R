
module1_txt <- list(
  "title"         = paste("Differences between $V_",NOT$total,"$, $V_",NOT$devI,"$ and 
                           $V_",NOT$error,"$, intercept",sep=""),
  "goal"          = "<b>Goal:</b> to develop understanding of hierarchies in 
                      variance when individuals express traits repeatedly.",
  "statModTitle"  = "<b>Statistical model</b>"
)

# Step 1 --------------
Mod1Step1_txt <- list(
  
  "title"      = "Step 1: Mean and variance when the trait is expressed once and does not change",
  "subgoal"    = "<b>Sub-goal:</b> Illustrating the concept of variance and mean, 
                  for traits with no within-individual variance but which is measured with error. ",
  "intro"      = paste("<b>Introduction:</b> This is the simplest possible situation. 
                        A trait is measured from a group of individuals in a population is 
                        characterised by a mean ($",NOT$mu,"$) and a variance ($V_",NOT$total,"$).",sep=""),
  "exercise"   = "<b>Exercise:</b> In this situation we assume that we only have only one value of the trait 
                  for each individual. You first have to decide how many individuals to measure.",
  "para1"      = paste("In every case that we measure something, we know that we are making an error 
                  in the measurement. This error is assumed to be non-directional and hopefully 
                  represents only a small portion of the total variance $V_",NOT$total,"$. 
                  Below play with the error term . 
                  Generally measurement error variance should not be high, ideally lower then 5% of 
                  the total variance (i.e. if $V_",NOT$total,"=1$ then $V_",NOT$error,"=0.05$), 
                  but of course some traits can be associated 
                  with much higher measurement error:",sep=""),
  
  "point"      = paste("<b>Point:</b> The variance of measures ($V'_",NOT$total,"$) is often higher than the variance 
                  of true individual variance ($V_",NOT$devI,"$) if there is measurement error 
                  (and there will always be some). The estimate of total variance includes both 
                  - unmeasured - individual differences ($V'_",NOT$devI,"$) and measurement error ($V'_",NOT$error,"$). 
                  Note that the estimated $V'_",NOT$total,"$ (total variance of sampled values) is often not equal to 1. 
                  This is because we have sampled from a population where the true total phenotypic 
                  variance that includes individual (biological) variance and measurement error 
                  variance is actually equal to 1. The only way to obtain the true total variance 
                  would to sample the entire population (which in the case of SQuID is infinite in size).",sep=""),
  
  "solutions"  = "<b>Solutions:</b> First work to reduce the measurement error; 
                  to do so, you need to know the magnitude of measurement error . 
                  This requires measuring individuals more than once. 
                  This is explained in step 2 of this module."   
)

# Step 2 --------------
Mod1Step2_txt <- list(    
  "title"     = "Step 2: Repeatability and measurement error",
  "subgoal"   = "<b>Sub-goal:</b> Learning how to estimate measurement error 
                  in traits varying solely among individuals.",
  "intro"     = "<b>Introduction:</b> In the previous step, you estimated the mean and the 
                  total variance of a sample of individuals measured once. From this you 
                  learned that some of the observed variation in measures might be due to measurement 
                  error, which occurs when the measured value deviates (for whatever reason) 
                  from the biological value. Here we detail the sampling design that you might 
                  use to directly estimate the magnitude of measurement error; we do this 
                  for a trait where the true value of the trait is constant for an individual 
                  (cf. a &lsquo;fixed&rsquo; rather than &lsquo;labile&rsquo; trait, e.g. structural 
                  size in adulthood).  
                  Measurement error can be estimated by measuring the same set of individuals 
                  multiple times (preferably in a blind and randomized order). Since their 
                  trait values are fixed, any deviation in measured values across measurements 
                  of the same individuals should be due to measurement error.",
  "exercise"    = paste("<b>Exercise:</b> To simulate this new situation, we advise using 
                  the same parameters as before, in terms of the number of individuals 
                  and measurement error ($V_",NOT$error,"$; expressed as a proportion of the total 
                  phenotypic variance, $V_",NOT$total,"$) so that you can compare your output with 
                  the previous step:",sep=""),
  "para1"   =  "You also need to enter how often each individual is measured. We call this 
                variable &lsquo;Number of trait expressions&rsquo;. For simplicity, we are assuming that 
                each individual is measured the same number of times. Because we are 
                interested in setting up a scenario where individuals are assayed repeatedly, 
                you should set a number equal or greater than two. Play around with how 
                often you measure each individual, because the number of repeated measurements
                per individual affects how well your estimated values approximate the true values.",
  "para2"   =  "As before, we present the output in the form of a collection of figures and tables 
                that shows you your estimated values (derived from a univariate mixed-effects model) 
                as well as the true values. Compare how the estimated values deviate from the true values; 
                play for example with the &ldquo;number of trait expressions&rdquo; to see how the number of 
                repeated measurements per individual affects your estimates and how much they 
                deviate from the true values.",
  "para3"   =  paste("The three density plots show the distribution of estimated total phenotypic variance $(V'_",NOT$total,")$, 
                the estimated variance among individuals $(V'_",NOT$devI,")$, and 
                the estimated variance within individuals $(V'_",NOT$error,")$. 
                It demonstrates graphically that $V'_",NOT$total,"=V'_",NOT$devI,"+V'_",NOT$error,"$.",sep=""),
  "point"   =  "<b>Point: </b>If you have inputted the same parameter values as for step 1, 
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
  "para6"   =  paste("The scatter plot shows how correlated the first and second 
                measurements are. In the absence of measurement error 
                (i.e. $V_",NOT$error,"=0$), all points align on a single line ($y=x$). 
                Play around with the settings of $V_",NOT$error,"$; the higher the value, 
                the more widely the data points get dispersed around $y=x$, i.e. 
                the higher the measurement error the lower our ability to 
                predict an individual&rsquo;s phenotype based on a previous measurement.
                Repeatability thus provides an assessment of how well one has measured a 
                particular phenotype. Repeatability can be used for several other purposes too, 
                so the way it is calculated may vary some. 
                We will develop some of these nuances later on.",sep="")  
  
)

# Step 3 --------------
Mod1Step3_txt <- list(  
  "title"     = "Step 3: Within- and among-individual variance",
  "subgoal"   = "<b>Sub-goal:</b> to illustrate hierarchical structure of variance when individuals 
                         express traits multiple times (a trait varies within-individuals).",
  "intro"     = "<b>Introduction:</b> Individuals express different values of a trait at 
                different times due to the influence of the environment. 
                Collecting two or more measures for each individual provides 
                several types of information. We have already seen that it 
                can allow estimation of measurement error. If the measurements 
                are spread over time, then you can also estimate how much 
                variance is due to the environment and how the phenotype 
                responds to the environment. Finally, repeated measurements 
                can measure whether differences among individuals are solely 
                due to differences in the environment at the time of measurement 
                versus to some other source, such as differences among 
                individuals that were generated before the period of measurement. ",
  "exercise"  = paste("<b>Exercise:</b> As in the two previous steps we are now generating a 
                        new group of individuals, but in addition to the variance caused by 
                        measurement errors ($V_",NOT$error,"$) we also choose the variance caused by 
                        individual differences ($V_",NOT$devI,"$) that exist throughout our simulated study. 
                        One component of the total variance 
                        ($V_",NOT$total,"$) is left once we remove both measurement error variance ($V_",NOT$error,"$) 
                        and individual variance ($V_",NOT$devI,"$). 
                        This new variance represents the population mean response to an environmental 
                        effect ($V_{",EQ3$mean1," ",EQ2$env1,"}$) on the phenotype of an individual. 
                        In this step, the environmental effect ($",EQ2$env1,"$) is considered as specific 
                        and unknown. A specific environmental effect means that each 
                        individual of the population will experience different environmental conditions 
                        than do other individuals. For instance, the intensity of intra- and inter-specific 
                        competition within a population will be perceived differently among individuals. 
                        An unknown environmental effect represents environmental values that are not 
                        measured thus cannot be included in the statistical analysis.
                        You can now try several combinations of $V_",NOT$error,"$ and $V_",NOT$devI,"$ 
                        to see how $V_{",EQ3$mean1," ",EQ2$env1,"}$ changes 
                        and how this affects the estimates of repeatability and variance components.",sep=""),
  "para1"    = "Once again you can calculate repeatability as",
  "point"    = paste("<b>Point:</b> We can now estimate a particular variance component of $V_",NOT$total,"$ that 
                        represents among-individual differences that are consistent through time. This variance is also 
                        in some circumstance the index for individual &ldquo;personality&rdquo; differences. The residual variance 
                        combines both measurement error ($V_",NOT$error,"$) and the variance caused by the unmeasured (unknown) specific 
                        environment to each individual ($V_{",EQ3$mean1," ",EQ2$env1,"}$). $V_{",EQ3$mean1," ",EQ2$env1,"}$ 
                        thus reflects plasticity to unknown environments.
                        You can now test whether a given number of individuals and a given number of repeated measures 
                        per individuals can affect your estimations.<br> 
                        Repeatability now does not represent the same thing as previously because the denominator 
                        of the ratio includes both measurement error and variance due to plasticity.
                        This new repeatability is not simply a measure of your skill at measuring phenotypes. 
                        Instead, it now is an estimate of a biological phenomenon, consistent individual 
                        differences (i.e.personality in the case for behaviour). This estimate is conservative because 
                        measurement error reduces the  estimate to be less than the true biological repeatability. 
                        Note also that in this particular simulation, the model gives an estimate of repeatability 
                        that depends on $V_",NOT$residual,"$: the sum of both $V_{",EQ3$mean1," ",EQ2$env1,"}$ and
                        $V_",NOT$error,"$. Measurement errors in this 
                        scenario are not separable from the plastic response of individuals to unmeasured 
                        environment. To calculate measurement error alone, you would have to collect 
                        more than one measure on the same trait for each individual at the same time 
                        (e.g. two persons can observe the behaviour of an individual on the same video recording).",sep="")
)


# Step 4 --------------
Mod1Step4_txt <- list( 
  "title"     = "Step 4: Explaining Environmental Variance",
  "subgoal"   = "<b>Sub-goal:</b> to explain unknown environmental variance.",
  "intro"     = paste("<b>Introduction:</b> In step 3, we introduced $V_{",EQ3$mean1," ",EQ2$env1,"}$ 
                as the variance caused by the environment. 
                We did not know what that effect was (it was unknown and unmeasured), 
                but often we can measure the environment and assess its influence on phenotype. 
                In this step, we demonstrate how that is done.",sep=""),
 "exercise"    = paste("<b>Exercise:</b> As before, we will generate a new group of individuals, 
                with phenotypic variance caused by measurement error ($V_",NOT$error,"$), 
                individual differences ($V_",NOT$devI,"$), 
                and specific and measured (i.e. known) environmental effects 
                ($V_{",EQ3$mean1," ",EQ2$env1,"}$) on the phenotype of an individual.<br> 
                As before, you can set $V_",NOT$error,"$, $V_",NOT$devI,"$  and $V_{",EQ3$mean1," ",EQ2$env1,"}$, 
                but we suggest you use the same values you did in Step 3.",sep=""),
  "para1"    = "At this point, we want to introduce the idea of &lsquo;analysis&rsquo; models. 
                These are equations that specify effects producing each individual data point. 
                In essence, the analysis model is an attempt to describe the real world. 
                In the made-up world of SQuID, these analysis models have 
                the potential of recreating it exactly. The real world is different, 
                and most of the lessons we will cover have to do with problems in 
                estimating terms in analysis models when much is unknown. 
                For now, we will specify a model that should recreate our simulated 
                set of effectscompletely (with the caveat that we are sampling from an infinite 
                population so observed values will differ from input values). The model is:",
  "para2"    = paste("where $",NOT$trait.1,"_{",NOT$time,NOT$ind,"}$ is the phenotype 
                measured at $",NOT$time,"^{th}$ time for the $",NOT$ind,"^{th}$ individual, 
                $",NOT$devI,"_",NOT$ind,"$ is the individual mean phenotype for the $",NOT$ind,"^{th}$ individual, 
                $",EQ3$mean1,"$ is the effect of measured 
                environment $",EQ2$env1,"$ on the measure of phenotype, 
                and $",NOT$error,"_{",NOT$time,NOT$ind,"}$ is the error made in that measurement.
                Note that $V_{",EQ3$mean1," ",EQ2$env1,"}=
                ",EQ3$mean1,"^2V(",EQ2$env1,")$ where $V(",EQ2$env1,")=1$.",sep=""),
  "para3"    = "A mixed statistical model estimates these parameters:",
  "para4"    = paste("Visually, you can see what $",EQ3$mean1,"$ 
                     is in the following graph:",sep=""),
  "para5"    = "The variance among individuals can still be visualized in this world by plotting 
                each individual&#39;s dataset and line.",
  "para6"    = paste("We encourage you to go back and play around with the magnitude of $V_",NOT$error,"$ 
                to see how it affects estimates of  $",EQ3$mean1,"$
                and $V_",NOT$devI,"$. You can also play around with the slope and the ratio of 
                $V_{",EQ3$mean1," ",EQ2$env1,"}$ and $V_",NOT$devI,"$ to better understand the effects.",sep=""),
  "point"    = "<b>Point:</b> This exercise introduced explanatory variables (also known as fixed effects). 
                Because individual is a &ldquo;random&rdquo; effect, this is thus a &ldquo;mixed effects&rdquo; model. 
                The fixed effect part is a linear regression. Even if this is all you want 
                to do with your data, it is important to understand that a sampling regime 
                in which individuals are measured more than once creates the need to do 
                linear regression within a mixed model. Although we do not focus much on 
                significance testing here, the structure of data collected in this simulation 
                strongly affects inferences based on hypothesis testing. More importantly, 
                the combination of random effects and fixed effects sets one up to investigate 
                a wide array of processes involved at one or more levels in this hierarchal 
                structure of among versus within-individual variance."
)
