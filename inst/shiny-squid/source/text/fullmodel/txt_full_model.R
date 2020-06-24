fullmodelTxt <- list(
  
  "paragIntro"         = "This table allow you to select the phenotypic equation model 
                          that you want to use to generate your data. Start by clicking on each of the parameters 
                          of the model you want to include to generate the phenotypes (column &rdquo;option&rdquo;). 
                          You can chose whether you simulate one or more traits: if you select the multivariate 
                          option you will be allowed to select the number of traits you want to simulate; 
                          Otherwise the default option is &rdquo;univariate&rdquo;. You will also have to enter 
                          the information relative to the covariance components between the traits. 
                          Once you have selected the model of your choice, click on &rdquo;enter inputs&rdquo;.",
  
  "lorem"              = "Lorem ipsum dolor sit amet, mei legimus disputando ei. Eu odio interpretaris sea, 
                          nemore denique voluptatibus has et. Ei vis ferri putent, ne iusto choro placerat mea, 
                          verear oportere elaboraret ius at. Eum mutat aperiam instructior id, 
                          ea mei rebum animal reprimique.
                          Nisl dolor temporibus no vis. Cum no soleat volutpat democritum. 
                          Te eligendi ullamcorper concludaturque vix, id usu consequuntur necessitatibus, 
                          est ea laudem verterem. Id prima harum tation pro. Mel libris doctus posidonium cu, 
                          sed modo meliore volutpat ea, ei singulis conceptam est. His an alterum prodesset definitionem, 
                          per id malis homero, impedit elaboraret cotidieque eu sea.",
  
  # Step by step page
  
  "fullModelSbyS_intro_1"     = "The full model of SQuID allows the user to generate data sets of 
                                  individuals repeatedly expressing phenotypes, for one or two traits, in uniform time. 
                                  The user has thus the flexibility to add different variance components that will form the 
                                  phenotype of the individual at each time step, and to set up a relative importance to each component. 
                                  SQuID also allows the user to collect a sample of the simulated phenotypes (i.e., the operational data set), 
                                  according to specific sampling design. Finally, the user has the opportunity 
                                  to download the operational data set for further analyses.",
  "fullModelSbyS_intro_2"     = "In this page, we will introduce to you step by step how to use our full module. 
                                In case you are already familiar with the full model we invite to switch to the 
                                full model express page, which is more straightforward.  ",
  
  
  # Step by step description
  
  "ModelDesciption_intro_1"     = "In this section, we will describe the model used to simulate individual 
                                  phenotype values and the model used to create the sampling design.",
  
  
  "SimModel_intro"      = "As a first step, we generate phenotype values of individuals that belong to a study population (i.e., replicate world). 
                          Phenotypic values of each trait are calculated using 
                          the phenotypic equation described below.",
  
  "modelEquation_1"     = paste("The equation represents the model that generated phenotypic values ($",EQ$phen.1,"$ or $",EQ$phen.2,"$) of 
                           two traits for each individual $",NOT$ind,"$ and at each instance of time $",NOT$time,"$, in each group $",NOT$group,"$.", sep=""),
  "modelEquation_2"     = paste("It should be noted that subscripts are organised to represent the different hierarchical 
                           levels involved in the generation of the value of the trait: subscript $",NOT$time,"$ stands for the 
                           instance at which the traits is measured for the individual $",NOT$ind,"$, within group $",NOT$group,"$. 
                           Finally subscript $",NOT$trait.1,"$ or $",NOT$trait.2,"$ refers to the two traits generated.", sep=""),
  "modelEquation_3"     = paste("At first sight we can see that the phenotypic value of the trait $",NOT$trait.1,"$ for individual $",NOT$ind,"$ 
                          at instance $",NOT$time,"$ in group $",NOT$group,"$ is the sum of a series of components. 
                          Let's first decompose the equation into all of its components, and we then will see how each of the 
                          components is generated:", sep=""),
  "modelEquation_4"     = paste("The first component on the right hand side of the equation represents the population average 
                          $",EQ$mean0.1,"$ for the trait $",NOT$trait.1,"$, and the deviation of each individual's trait mean 
                          from that population average ($",EQ$dev0.1,"$, generally called individual level or individual intercept). 
                          All the other elements occurring on the right hand side of the equation represent also deviations 
                          from the average of the trait in that population.", sep=""),
  "modelEquation_5"     = paste("Let's now go to the end of the equation, where we find the residual $",EQ$error.1,"$. 
													The residual term represents unaccounted effects on the phenotype which could be biological effects and/or measurement errors. 
													At the left of the 
                          residual term we have $",EQ$group.1,"$ that represents the deviation from the population average of 
                          trait $",NOT$trait.1,"$ for a group $",NOT$group,"$ (this means that all the individuals of a given group $",NOT$group,"$ share 
                          the same value of $",NOT$groupV,"$ ($",NOT$groupV,"$ follows a normal distribution with a mean = 0 and variance = $Var(",NOT$groupV,")$).
                          The concept of grouping is wide ranging from genetic similarities (e.g. families) to spatial aggregations 
                          (e.g. territories) and diet compositions and could be specific to the each study.",sep=""),
  "modelEquation_6"     = paste("The last three components $(",EQ$mean1.1,"+",EQ$dev1.1,").",EQ$env1,"+(",EQ$mean2.1,"+",EQ$dev2.1,").",EQ$env2," 
                          + (",EQ$mean12.1,"+",EQ$dev12.1,")",EQ$env12,"$ represent two different environmental effects 
                          and their interaction that you could use to generate different sources of variation 
                          for your simulations.",sep=""),
  "modelEquation_7"     = paste("These terms represent phenotypically plastic changes in the value of the trait as a function 
                          of two environmental factors $",NOT$env,"_1$ and $",NOT$env,"_2$. In other words, that part of the equation is 
                          the reaction norm of trait $",NOT$trait.1,"$ as a function of either environment $",NOT$env,"_1$ or $",NOT$env,"_2$ 
                          (or both $",NOT$env,"_1$ and $",NOT$env,"_2$). Below we will explain the equation for a given environmental 
                          factor $",NOT$env,"$. Environment $",NOT$env,"$ can be measured and thus you can decide whether or not to include $",NOT$env,"$ 
                          in subsequent mixed-effects models (whether you include $",NOT$env,"$ in the model or not allows you to 
                          test how unknown/unmeasured environments affect estimates of other parameters in the model). 
                          The reaction norm is itself composed of a population-level component (e.g. $",EQ$mean1.1,"$) and of 
                          an individual deviation from the population norm ($",EQ$dev1.1,"$). 
                          When both terms are combined trait values are generated so that individuals differ 
                          in their slopes for trait $",NOT$trait.1,"$ as a function of environment $",NOT$env,"$. 
                          Lastly there is a possibility to implement an interaction between $",NOT$env,"_1$ and $",NOT$env,"_2$ 
                          [$(",EQ$mean12.1,"+",EQ$dev12.1,")",EQ$env12,"$].",sep=""),
  
  "indSpecResponses"    = paste("Below we show the way we generate the different values of each individual-level random effect for 
                          both traits $",NOT$trait.1,"$ and $",NOT$trait.2,"$. The vector of individual deviation of the intercept and slopes for 
                          the two traits follows a multivariate normal distribution (MNV) with a zero-mean and covariance/variance 
                          matrix structure $\\Omega_{",NOT$devI,NOT$devS,"}$. On the right hand side is shown (co)variance matrix 
                          $\\Omega_{",NOT$devI,NOT$devS,"}$. Values of variance stand along the diagonal of the matrix (i.e., respectively, 
                          the among-individual variance in intercepts for $",NOT$trait.1,"$ $(Var(",NOT$devI,"_",NOT$trait.1,"))$, the among-individual variance in slopes 
                          for $",NOT$trait.1,"$ $(Var(",NOT$devS,"_",NOT$trait.1,"))$, the among-individual variance in intercepts for $",NOT$trait.2,"$ $(Var(",NOT$devI,"_",NOT$trait.2,"))$ 
                          and the among-individual variance in slopes for $",NOT$trait.2,"$ $(Var(",NOT$devS,"_",NOT$trait.2,"))$). 
                          Covariance values between intercepts (e.g. $Cov(", EQ2$dev0.1, ",", EQ2$dev0.2,")$), 
                          slopes (e.g. $Cov(", EQ2$dev1.1, ",", EQ2$dev1.2,")$), 
                          and intercepts and slopes (e.g. $Cov(", EQ2$dev0.1, ",", EQ2$dev1.1,")$) lie below the diagonal. 
                          When building up phenotypic design the program lets you specify all of these (co)variance values. 
                          Note that in SQuID inputs the correlation/variance matrix is asked instead of the (co)variance matrix. 
                          This is more convenient for users while correlations range between -1 and 1.",sep=""),
  "environment_1"       = "SQuID allows you to generate different structures by which the environment changes over time: 
                          <ol><li>stochastic</li> <li>temporally auto-correlated</li> <li>linear</li> <li>cyclic</li></ol>",
  "environment_2"       = paste("For the first structure (<b>stochastic</b>), environmental values $",NOT$env,"_{",NOT$random,NOT$time,"}$ are 
                          generated using a Normal distribution with mean 0 and variance $Var(",NOT$env,"_",NOT$random,")$. 
                          $$",NOT$env,"_{",NOT$random,NOT$time,"}\\sim N(0,Var(",NOT$env,"_",NOT$random,"))$$
                          This means that environmental values will be totally 
                          stochastic and uncorrelated from one time point to the next. This simple situation is not the most realistic 
                          (i.e., it is hard to think of an environmental factor that is completely stochastically distributed in time). 
                          The other options are more complex but also more realistic.",sep=""),
  "environment_3"       = paste("For the <b>temporally auto-correlated</b> $",NOT$env,"_{",NOT$autocorrelated,NOT$time,"}$ 
                          we assume that two values of the environment close in time 
                          are more similar than two values further apart from each other (i.e., positive autocorrelation). 
                          Environmental values $",NOT$env,"_{",NOT$autocorrelated,NOT$time,"}$ are generated 
                          as the product of a stochastic value by a decay function.
                          $$",NOT$env,"_{",NOT$autocorrelated,NOT$time,"}\\sim N(0,Var(",NOT$env,"_",NOT$random,"))\\times e^{-\\alpha\\Delta ",NOT$time,"}$$                          
                          where $\\Delta ",NOT$time,"$ is the time interval between two instances of the study period, and $\\alpha$
                          is the decay rate, a measure of how fast the correlation decays with time.
                          $$\\alpha=ln(autocor)$$
                          where $autocor$ is the desired correlation between two successive values.",sep=""),
  "environment_4"       = paste("Once the <b>linear</b> trend is set up SQuID generates 
                          a series of values following a classical linear equation.
                          $$",NOT$env,"_{",NOT$linear,NOT$time,"}=a+b.",NOT$time,"$$
                          where $a$ and $b$ are respectively, the intercept and the slope of the regression between 
                          $",NOT$env,"_{",NOT$linear,NOT$time,"}$ and $",NOT$time,"$.  
                          This option allows you to generate environmental values that can change linearly with time, 
                          for example mimicking phenology during a season or long-term environmental changes.",sep=""),
  "environment_5"       = paste("If you are interested in mimicking daily or seasonal fluctuations you can choose a <b>cyclic</b> 
                          (sinusoidal) environmental variation, according to a sinusoidal equation.
                          $$",NOT$env,"_{",NOT$cyclic,NOT$time,"}=a\\times sin(b.",NOT$time,"+c)+v$$
                          where $\\lvert a\\rvert$ is the amplitude, 
                          $2\\pi/\\lvert b\\rvert$ the period, 
                          $-c/\\lvert b\\rvert$ the horizontal shift, 
                          and $v$ the vertical shift.
                          You can therefore change the shape of the cyclic curve by setting the different parameters.",sep=""),
  "environment_6"       = "By adding up the different options together you can create environmental data with combinations of effects. 
                          Adding the four options together (the most complex situation) will generate environmental data 
                          with a linear trend, cyclicity, autocorrelation and stochasticity.",
  "environment_7"       = paste("The environment could be also <b>shared</b> or <b>unshared</b> among individuals. 
                          A shared environment between individuals means that it is general to all the individuals 
                          within the population. All individuals will experience the same environment even 
                          if the environment would vary over time. In this case, only one environment ($",NOT$env,"$) 
                          will be generated using the different options described above. 
                          An unshared environment between individuals means that each individual 
                          will experience a specific environment. For instance, if we have 3 individuals 
                          in our population 3 different environments $",NOT$env,"_{",NOT$ind,"=a}$, $",NOT$env,"_{",NOT$ind,"=b}$, and $",NOT$env,"_{",NOT$ind,"=c}$ will be generated for 
                          respectively the individual a, b, and c. Although these environments are different, 
                          they follow the same general trend defined by environment options described 
                          previously (stochastic, auto-correlated, linear, and cyclic). 
                          In order to create multiple environments from one environment configuration, 
                          we apply variability around each parameter (e.g. the intercept of the 
                          linear effect or the amplitude of the cyclic effect) that follows 
                          a normal distribution.",sep=""),
  "environment_8"       = paste0("Finally, in SQuID the final output of each 
													environmental effect ($",EQ2$env1,"$, $",EQ2$env2,"$ and $",EQ2$env12,"$) is 
													expressed in unit variance (i.e., $Var(",NOT$env,")=1$) and mean-centered (i.e., $E(",NOT$env,")=0$)."),
  "groupingNerror"      = paste("The high-level grouping term ($",NOT$groupV,"$) is generated from 
                          a normal distribution of mean 0 and variance $Var(",NOT$groupV,")$, 
                          and the residual term ($",NOT$error,"$) from a normal distribution 
                          of mean 0 and variance $Var(",NOT$error,")$.",sep=""),
  
  
  "samplingDesign_1"    = "As a second step, we retrieve a subsample of previously simulated phenotypic values according 
                          to a specific sampling design. The sampling design between individuals 
                          could vary by multiple parameters such as the number of records sampled, 
                          and the instance of those records.",
  "samplingDesign_2"    = paste("First we define the duration of the sampling period that is the same for all individuals.
                          $$t_{sampling}=t_{total}\\times(1-",general_VAR$btwIndVarTimSamp,")$$
                          where $t_{total}$ is the total simulation duration and $",general_VAR$btwIndVarTimSamp,"$ 
                          is the among-individual variance in timing of sampling (between 0 and 0.95).  
                          $",general_VAR$btwIndVarTimSamp,"$ is used to control the spreading intensity 
                          in sampling time between individuals (figure 1). 
                          When $",general_VAR$btwIndVarTimSamp,"$ is small (e.g. 0.1) $t_{sampling}$ is large 
                          (90% of total simulation duration) and all individuals are sampled almost fullmodelTxt
                          within the same period. Note that when $",general_VAR$btwIndVarTimSamp,"$ is 0 all individuals 
                          are sampled with the entire period of the simulation. 
                          Contrarily, when $",general_VAR$btwIndVarTimSamp,"$ is large (e.g. 0.9) $t_{sampling}$ is small 
                          (10% of total simulation duration) and each individual sampling period is spread out
                          within the total simulation duration following a uniform distribution.",sep=""),
  "samplingDesign_3"    = paste("The number of sampled records for each individual ($NR_",NOT$ind,"$) is generated following 
                          a poisson distribution.
                          $$NR_",NOT$ind,"\\sim poisson(NR)$$
                          where $NR$ is the mean number of records.",sep=""),
  "samplingDesign_4"    = "Finally, the exact times of sampling for each individual are generated according 
                          to a uniform distribution within each individual sampling period.",
  
  # Step by Step summary section
  "bivariateStepbyStep" = paste0("SQuID can model a bivariate phenotypic equation with random intercepts 
                                 and slopes for individual identity with respect to two environmental covariates. 
                                 SQuID enables both linear and interactive effects of these covariates on the 
                                 bivariate phenotype (i.e., non-warped vs. warped multi-dimensional reaction 
                                 norm planes; <a href='http://onlinelibrary.wiley.com/doi/10.1111/2041-210X.12430/abstract' target='_blank'>Araya-Ajoy <i>et al.</i> 2015</a>; 
                                 <a href='http://onlinelibrary.wiley.com/doi/10.1111/brv.12131/abstract' target='_blank'>Westneat <i>et al.</i> 2015</a>) to be modelled. 
                                 Covariances in SQuID emerge due to user-defined settings in the fixed and 
                                 random parts of the equation. Covariances in the random part are set directly 
                                 by the user, whereas covariances in the fixed part instead emerge when 
                                 a focal environmental effect is set to affect both traits (e.g., $",EQ$mean1.1,"\\neq 0$ and $",EQ$mean1.2,"\\neq 0$). 
                                 A large number of covariance terms, listed here, can thus be simulated."),
  
  # Step by Step bivariate section
  "summaryStepbyStep" = paste0("In the following table, we summarized the components that can be extracted from a mixed-effects model, 
                               using a data set generated by SQuID in a situation where the phenotypic trait y 
                               is a function of two different environmental variables ($",EQ2$env1,"$ and $",EQ2$env2,"$) and their interaction.
  														 The phenotype trait $",NOT$trait.1,"$ is expressed for each individual $",NOT$ind,"$ and at each instance of time $",NOT$time,"$, in each high-level group ",NOT$group,"."),
  
  # Step by Step Input section
  "inputStepbyStepDesign" = "In this section you will configure your simulation and sampling design by entering your 
                            desired parameter values. All the parameters have default values that you 
                            can decide to modify or not.",
  "inputSimDesign"        = "You have to start by generating your world by setting the duration of your study. 
                            The default is set to 100 time steps but you can generate less or more time steps. 
                            You can also select the number of replicated populations you want to generate 
                            (note that data from different populations will be saved in the same output 
                            file that you could later use to run statistical analyses).
                            The replicated populations are generated independently using the 
                            same simulation design that has been initially inputted by the user. 
                            The second step is to decide how many individuals will you create in each sampled population, 
                            how many traits you will create for each individual (max = 2), 
                            and how many groups of individuals will you define within each population.
                            Note that the number of individuals has to be divisible by the number of groups.",
  "inputEnvironment_1"    = paste("You have now to decide what types of environmental effects you will generate for the simulated traits. 
                            In this section you can select a maximum of two environment effects ($",EQ2$env1,"$ and $",EQ2$env2,"$). 
                            These environments can be customized according to the various options 
                            displayed in their respective tabs. You can add stochastic, 
                            linear and cyclic effects to the general structured environmental effects. 
                            Each option can be added or deleted from the general environmental 
                            structure by respectively checking or unchecking its associated checkbox.",sep=""),
  "inputEnvironment_2"    = "For the <b>stochastic</b> effect you have to specify the variance for that environment. 
                            This variance will create stochastic variation in the environmental value from one time step to the next. 
                            Furthermore, within the stochastic environmental effect section, it is possible to add <b>auto-correlated</b> 
                            effects that follow the algorithm presented in the &ldquo;Description&rdquo; page. You have to specify the 
                            correlation value ranging from 0 to 1 that characterizes the magitude of temporal autocorrelation.",
  "inputEnvironment_3"    = "You can also choose whether or not the environment is showing a <b>linear</b> or a <b>cyclical</b> 
                            temporal trend. One more time, you have to specify the parameter values for each 
                            option such as the intercept and the slope for the linear effect and the amplitude, 
                            the period, the horizontal shift and the vertical shift for the cyclic effect.
                            Therefore, if you select the three options you can create linearly increasing 
                            environmental values with some cyclic and stochastic variation from time to time.",
  "inputEnvironment_4"    = "For each environment type (stochastic, linear and cyclic), you can also choose whether that 
                            environmental effect is shared or not between individuals by checking or not 
                            the &ldquo;Shared environment&rdquo; checkbox. For instance, you could decide to share 
                            the stochastic and linear environmental effects between individuals while the 
                            cyclic effect is not shared. When an environment effect is not shared 
                            between individuals an input cell appear showing the environmental 
                            variance value used to generate the different environments. 
                            These individual-specific environments are created by varying their parameters 
                            with a normal distribution where the mean is the parameter value itself and 
                            the variance is the specified environmental variance and that for each 
                            environment effect type.",
  "inputEnvironment_5"    = paste0("Furthermore, below each environmental option a graph of the general environmental 
                            structure is displayed in order to facilitate the environment visualisation 
                            before running the simulation. Note that when the environment is shared (general to all individuals) only 
                            one environmental pattern is displayed. In contrast, when the environment 
                            is not shared (specific to each individual) multiple environment patterns (one for each individual) 
                            are instead displayed in different colours. 
                            To understand how the environment generator works we suggest to start 
                            with simple environmental structures such as only linear or cyclic and 
                            then add further complexity by combining multiple environmental effect types.
  													Also, remember that in SQuID the final output of each 
  													environmental effect ($",EQ2$env1,"$, $",EQ2$env2,"$ and $",EQ2$env12,"$) is 
  													expressed in unit variance (i.e., $Var(",NOT$env,"=1)$) and mean-centered (i.e., $E(",NOT$env,")=0$).
  													However, the previsualization graph below displays the generated environmental data before standardization."),
  "inputEnvironment_6"    = paste("Finally, you can add a third environmental effect corresponding to the 
                            interaction between $",EQ2$env1,"$ and $",EQ2$env2,"$. 
                            Note that an environmental interaction is allowed only when $",EQ2$env1,"$ 
                            and $",EQ2$env2,"$ are selected.",sep=""),
  "inputEnvironment_7"    = "For the analysis part, these environmental effects can be known or unknown to the researcher.
                            For the known environment we assume that you have been able to measure 
                            the environmental values and thus will be able to use them for further analyses. 
                            Unknown environments, in contrast, represent environments that 
                            are not measured. This corresponds to the cumulative effects 
                            of the many environmental factors on your trait that you did not 
                            have the opportunity to measure but that are still affecting it.",
  
  
  "inputEquation"        = "You are not yet at the stage where you generate the phenotypic values of the trait(s). 
                            Note that the program gives you the phenotypic equation of the model you just set up. 
                            Any change in the parameters of the model will lead to a change in 
                            the equation that is shown below.",
  
  "inputPhenDesign"      = "The section below allows you to enter all the information on the variance components 
                            affecting the phenotypic values of the trait(s). It is a dynamic section that 
                            changes according to the parameters you have included earlier in the model. 
                            For example when you choose to simulate two traits you obtain a matrix of 
                            variance/correlation for the two traits taht otherwise does not appear. 
                            Depending on the model you have specified 
                            earlier some of the matrix cells will appear in colour. 
                            These are the variance and correlation components that you can choose 
                            to specify according to the model you have chosen before.
                            The section is organised so that you can specify population mean values on top, 
                            and then individual variance/correlation values. At the bottom of the window you 
                            have the option to add some residual and grouping variance.
                            Please pay attention that in the matrix below, the <b>correlation</b> between each component 
                            pair has to be entered and not the covariance.",
  
  "inputVarSummary"      = "Congratulation you just got your simulation design configured. 
                            At this point you can visualize a summary of all the variances  and covariances 
                            of the model that you just defined. 
                            This summary table contains the value of each variance in addition to its proportion 
                            according to the total phenotypic variance.",
  
  "inputSamplingDesign_1"= "Now you have simulated populations of individuals each with trait values for each time step. 
                            It is time to sample as a biologist would by capturing and testing or observing 
                            individuals for a limited amount of time during the study period. 
                            To mimic this situation SQuID offers several options related to the 
                            sampling design you want to use.",
  "inputSamplingDesign_2"= "In the top left hand side of the sampling design section you have to enter the average 
                            number of records per trait. This number has to be between 1 and the number of 
                            time steps of the study period. SQuID does not let you specify other numbers. 
                            In the input cell underneath you can enter a value for the among-individual 
                            variance in timing of sampling (between 0 and 0.95); a variance of zero means 
                            that you have no among-individual differences in the timing of sampling. 
                            An increase in the variance leads to a decrease in the overlap between 
                            individual samples thus a decrease in the time of sampling for each individual.",
  "inputSamplingDesign_3"= "On the right hand side of the section, there are little check boxes that 
                            allow you to indicate whether you want the same or different numbers of 
                            records per individual, and if you want individuals to be sampled at the 
                            same or different times (when you have two traits you can specify 
                            whether you want records for the two traits at the same or different 
                            times and the same or different number of records for the two traits). 
                            Note that if you click on the box untitled &ldquo;same sampling time among 
                            individuals&rdquo; SQuID will automatically consider that the number of 
                            sampled records is the same between individuals in addition that records are taken
                            at the same time.",
  "inputSamplingDesign_4"= paste0('Finally, SQuID offers a previsualization of the sampling design. 
                            The figure shows when the trait $',NOT$trait.1,'$ of the first 5 individuals will be sampled. 
                            The preview figure will be updated automatically when the user changes one 
                            of the sampling parameters. Note that the sampling design displayed is 
                            just one of many possible examples. We suggest to press the "Refresh" button 
                            in order to visualize other possible examples of sampling designs that are generated 
                            according to the rules defined by the entered parameters.'),
  
  "inputRun"             = "It is now time to run your simulation and to check the output in the output tab. 
                            Note that if any inputs have been improperly entered the run button will be 
                            inaccessible and it will not be possible to run the simulation.
                            For instance, this could happen if you enter a correlation value in the 
                            correlation/variance matrix that is out of the authorized range (between -1 and 1) 
                            or if the number of sampled records is higher than the duration of the sampling period. 
                            In that case you should fix all the input errors and then run 
                            the simulation. ",
  
  "inputRcode"          = "Note that simulations can also be run directly in R using our function <i>squidR()</i>. 
													 You can download the R code required to run the current 
													 simulation that is based on the parameter values you have entered.",   
  
  "output_1"            = "While you are running the simulation, SQuID is bringing you to the output tab. 
                          Here you have the opportunity to download generated (raw and sampled) 
                          data or to inspect figures showing different components of 
                          your simulation such as the environment patterns, 
                          the individual phenotypic values, and the sampling 
                          events over the simulation time. ",
  "output_2"            = "The first section contains figures that show how the different environmental parameters 
                          are distributed in time. When you do not set up any environment 
                          parameters the figure show a flat line.",
  "output_3"            = "If you click on the &ldquo;Individual Phenotypes&rdquo; option you can access to top figure 
                          showing the complete set of values for each individual at each time step, 
                          and the bottom one showing the values sampled for each individual according 
                          to your sampling design.",
  "output_4"            = "Finally by clicking on the &ldquo;Sampling time&rdquo; option you can see a figure illustrating 
                          the distribution of sampling occasions 
                          for each individual through time.",
  "output_5"            = "We also provide a brief description of the generated data, 
                          either raw data or sampled data, which are available to 
                          download as a .csv files.",
  "output_figure_color" = paste0("The figures below show the data related to one trait ($",NOT$trait.1,"$) and one 
                                replicate of the 20 first individuals of the simulated population."),
  
  
  
  
  
  
  
  
  
  
  "last" = "last"
  
  

)