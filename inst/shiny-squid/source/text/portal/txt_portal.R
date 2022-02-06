portal_txt <- list(
      "parag0_title"         = "SQuID goal",
      "parag0_contents_1"    = "<b>SQuID</b> stands for <b>S</b>tatistical <b>Qu</b>antification of <b>I</b>ndividual <b>D</b>ifferences and is the 
                                product of the SQuID working group. The package aims to help scholars who, 
                                like us, are interested in understanding patterns of phenotypic variance. 
                                Individual differences are the raw material for natural selection to act 
                                on and hence the basis of evolutionary adaptation. Understanding the sources 
                                of phenotypic variance is thus a most essential feature of biological investigation 
                                and mixed effects models offer a great, albeit challenging tool. 
                                Disseminating the properties, potentials and interpretational challenges 
                                in the research community is thus a foremost goal of SQuID.",
      "parag0_contents_2"    = "The squid package has two main objectives: First, it provides an educational 
                                tool useful for students, teachers and researchers who want to learn to use 
                                mixed-effects models. Users can experience how the mixed-effects model framework 
                                can be used to understand distinct biological phenomena by interactively 
                                exploring simulated multilevel data. Second, squid offers research opportunities 
                                to those who are already familiar with mixed-effects models, as squid enables 
                                the generation of datasets that users may download and use for a range of simulation-based 
                                statistical analyses such as power and sensitivity analysis of multilevel and multivariate data.",
      "parag1_title"         = "SQuID biological goals",
      "patag1_image"         = '<img id="logo" src="pictures/squid_logo.png" align="left" alt="SQuID" width=120px heigth=120px>',
      "parag1_contents"      = 'SQuID seeks to understand patterns of phenotypic variance, which is the material 
                                on which natural selection is acting, and thus is a most essential feature of 
                                biological investigation. Different sources of variations are at the origin of 
                                the phenotype of an individual. Individuals differ in their phenotypes because 
                                they have different genes. They also experience different types of environmental 
                                effects during their lifetime. Some are imposing a very permanent mark on the 
                                phenotype over the whole lifetime. For example, by their parental behaviour 
                                individuals can affect their offspring phenotypes permanently, causing among-individual 
                                variation. Other environmental sources play more short-term effects on the phenotype, 
                                as individuals react in the plastic way to these sources, causing within-individual variation. 
                                The patterns of variation can be very complex. For instance individuals differ not only 
                                in their average phenotypes but also in how they can change their phenotype according to 
                                changes in the environment, which represents an interaction between the among- and the 
                                within-individual levels. Selection can act differently on these different components of 
                                variance in the phenotypes of a trait, and this is why it is important to estimate them. 
                                Mixed models are very flexible statistical tools that provide a way to estimate the 
                                variation at these different levels, and represent the general statistical framework 
                                for evolutionary biology. Because of the progress in computational capacities mixed 
                                models have become increasingly popular among ecologists and evolutionary biologists 
                                over the last decade. However, running mixed model is not a straightforward exercise, 
                                and the way data are sampled among and within individuals can have strong implications 
                                on the outcome of the model. This is why we considered it was necessary to produce a 
                                simulation tool that could help new users interested in decomposing phenotypic variance 
                                to get more familiar with the concept of hierarchical organization of traits, with mixed 
                                models and to avoid pitfalls caused by inappropriate sampling.',
      
      "background_title"     = "Background",
      "background_content_1" = "The phenotype of a trait in an individual results from a sum of genetic and environmental 
                                influences. Phenotypic variation is structured in a hierarchical way and the hierarchical 
                                modeling in mixed effect models is great tool to analyze and decompose such variation. 
                                Phenotypes vary across species, across populations of the same species, across individuals 
                                of the same population, and across repeated observations of the same individual. 
                                We focused on the individual level because it represents one of the most important 
                                biological levels to both ecological and evolutionary processes. Different sources 
                                of variation are at the origin of the phenotype of an individual. Individuals may 
                                differ in their phenotypes because they carry different gene variants (i.e. alleles). 
                                But individuals also experience different environments during their lifetime. 
                                Some environmental influences impose a lasting mark on the phenotype, while others are more ephemerous. 
                                The former tend to produce long-lasting, among-individual variation, while the latter 
                                causes within-individual variation. However, this depends on the time scale at 
                                which the measurements of the phenotypes are done relative that of the environmental 
                                influences. Furthermore, individuals differ not only in their average phenotypes 
                                but also in how they respond to changes in their environment 
                                (i.e. differences in individual phenotypic plasticity). 
                                This represents an interaction between the among- and the within-individual levels of variation. 
                                The patterns of variation can, thus, be very complex. Selection can act differently on these different 
                                components of variance in the phenotypes of a trait, and this is why it is important to quantify their magnitude.",
      "background_content_2" = "Mixed models are very flexible statistical tools that provide a way to estimate the 
                                variation at these different levels, and represent the general statistical framework for evolutionary biology.
                                Because of the progress in computational capacities mixed models have become increasingly 
                                popular among ecologists and evolutionary biologists over the last decade. However, 
                                fitting mixed model is not a straightforward exercise, and the way data are sampled among 
                                and within individuals can have strong implications on the outcome of the model. 
                                This is why we created the squid simulation tool that could help new users interested 
                                in decomposing phenotypic variance to get more familiar with the concept of hierarchical 
                                organization of traits, with mixed models and to avoid pitfalls caused by inappropriate sampling.",
      "parag2_title"         = "History of the project",
      "parag2_contents"      =  "It all started in Hannover in November 2013 at the occasion of a workshop on 
                                personality organised by Susanne Foitzik, Franjo Weissing, and Niels Dingemanse and funded 
                                by the Volkswagen Foundation. During this workshop, a group of researchers discussed the 
                                potential issues related to sampling designs on the estimation of components of the phenotypic 
                                variance and covariance. It became obvious that there was an urgent need to develop a 
                                simulation package to help anyone interested in using a mixed model approach at getting 
                                familiar with this methods and avoiding the pitfalls related to the interpretation of the results. 
                                A first model and a working version of the package were created in January 2014, 
                                during a meeting at Universit&eacute; du Qu&eacute;bec &agrave; Montr&eacute;al. The current version was produced during a workshop 
                                in November 2014, at the Max Plank Institute for Ornithology in Seewiesen.",
      "parag3_title"         = "Brief description of modules",
      "parag3_contents1"     = "<b>SQuID</b> is made to help researchers to become familiar with multilevel variation, and to 
                                build up sampling designs for their study. SQuID is built up as a series of modules that guide 
                                the user into situations of increasing complexity to explore the dynamics between the way 
                                records are collected and estimates of parameters of specific interest; The last module 
                                is the <b><i>full model simulation package</i></b> that allows the user to generate data sets that can then be 
                                used to run analyses in the statistical package of their choice for specific research questions.",
      "parag3_contents2"     = paste0("<b>SQuID</b> is based on a mathematical model that creates a group of individuals (i.e. study population) 
                                repeatedly expressing phenotypes, for one or different traits, in uniform time. Phenotypic 
                                values of traits are generated following the general principle of the phenotypic equation 
                                (<a href='http://onlinelibrary.wiley.com/doi/10.1111/1365-2656.12013/abstract' target='_blank'>Dingemanse & Dochtermann 2013, Journal of Animal Ecology</a>): 
                                phenotypic variance ($V_",NOT$total,"$) is assumed to be the sum of a series of components (see the full model). 
                                The user has thus the flexibility to add different variance components that will form the phenotype 
                                of the individual at each time step, and to set up the relative importance of each component. 
                                SQuiD then allows the user to collect a subsample of phenotypes for each simulated individual 
                                (i.e. operational data set), according to a specific sampling design. For most of the modules, the 
                                operational data set generated is automatically fed into a statistical model in R and the main results 
                                of the analysis shown in an output. For the full model the user has the opportunity to download 
                                the operational data set for further analyses."),
      "parag4_title"         = "SQuID team",
      "parag4_contents"      = "Hassen Allegue (Universit&eacute; du Qu&eacute;bec &Agrave; Montr&eacute;al, Montreal, Canada)<br>
                                Yimen G. Araya-Ajoy (Norwegian University of Science and Technology, Trondheim, Norway)<br>
                                Niels J. Dingemanse (Max Planck Institute for Ornithology, Seewiesen & University of Munich, Germany)<br>
                                Ned A. Dochtermann (North Dakota State University, Fargo, USA)<br>
                                Laszlo Z. Garamszegi (Estaci&oacute;n Biol&oacute;gica de Do&ntilde;ana-CSIC, Seville, Spain)<br>
                                Shinichi Nakagawa (University of New South Wales, Sydney, Australia)<br>
                                Denis R&eacute;ale (Universit&eacute; du Qu&eacute;bec &Agrave; Montr&eacute;al, Montreal, Canada)<br>
                                Holger Schielzeth (University of Bielefeld, Bielefeld, Germany)<br>
                                David F. Westneat (University of Kentucky, Lexington, USA)<br>",
      "beginners"            = paste0("The SQuID modules are designed for users who have some but not a 
                                lot of statistical background, particularly with linear mixed models. 
                                We strongly recommend that if you are in this category, you begin with 
                                the module 	&ldquo;Basic Lessons&rdquo;. That should be followed by the module 
                                &ldquo;Non-stochastic environments&rdquo;. You will need to be very comfortable with 
                                the ideas here before moving on. Which module you choose next depends 
                                on your interests but Step 1 in module &ldquo;Multidimensional Plasticity&rdquo; 
                                introduces multiple regression. The module &ldquo;",Module_titles$mod6,"&rdquo; 
                                may also be good to do after module &ldquo;Non-stochastic environments&rdquo;."),
      "teachers"             = "The SQuID modules can be very useful for teaching statistical concepts, 
                                especially ones related to linear mixed models. Which module to use 
                                depends on your students and what you want them to learn. 
                                Brief descriptors of each module are available on this page 
                                (instructions to see them). We also recommend that you skim some of 
                                the modules or visit the full equation step-by-step page to better 
                                understand how SQuID works.",
      "experts"              = "SQuID was designed to provide a user-friendly and web-based program 
                                to simulate data for testing a variety of ideas about sampling and 
                                bias in hierarchical mixed modeling. For those very familiar with 
                                these approaches and curious about SQuID, we recommend initially 
                                using the module &ldquo;Full model&rdquo; and the option &ldquo;Step-by-step&rdquo;. 
                                Once you understand how SQuID works, the &ldquo;Express model&rdquo; version 
                                will work best. Finally, we have SQuID available as an R function 
                                &ldquo;squidR()&rdquo; for those interested in doing efficient simulations.",
      "references_title"    = "References",
      "references_content"  = "Allegue, H., Araya-Ajoy, Y.G., Dingemanse, N.J., Dochtermann N.A., Garamszegi, 
                              L.Z., Nakagawa, S., R&eacute;ale, D., Schielzeth, H. and Westneat, D.F. (2016). 
                              SQuID - Statistical Quantification of Individual Differences: an educational 
                              and statistical tool for understanding multi-level phenotypic data 
                              in the mixed modelling framework. Methods in Ecology and Evolution, 
                              8:257-267.<br><br>
                              Dingemanse, N.J. and Dochtermann N.A. (2013). Quantifying individual variation in behaviour: 
                              mixed-effect modelling approaches. Journal of Animal Ecology, 82:39-54."
      

)