#' SQUID - Statistical Quantification of Individual Differences: an educational and statistical package for understanding multi-level phenotypic data in the mixed modelling framework.
#'
#' This package is the product of the SQuID group. SQuID stands for \strong{S}tatistical \strong{Qu}antification of \strong{I}ndividual 
#' \strong{D}ifferences. We seek to understand patterns of phenotypic variance, which is the material on which 
#' natural selection is acting, and thus is a most essential feature of biological investigation. 
#' Different sources of variations are at the origin of the phenotype of an individual. 
#' Individuals differ in their phenotypes because they have different genes. They also 
#' experience different types of environmental effects during their lifetime. Some are 
#' imposing a very permanent mark on the phenotype over the whole lifetime. For example, 
#' by their parental behaviour individuals can affect their offspring phenotypes permanently, 
#' causing among-individual variation. Other environmental sources play more short-term effects on the phenotype, 
#' as individuals react in the plastic way to these sources, causing within-individual variation. 
#' The patterns of variation can be very complex. For instance individuals differ not only in their 
#' average phenotypes but also in how they can change their phenotype according to changes in the environment, 
#' which represents an interaction between the among- and the within-individual levels. 
#' Selection can act differently on these different components of variance in the phenotypes of a trait, 
#' and this is why it is important to estimate them. Mixed models are very flexible statistical tools that 
#' provide a way to estimate the variation at these different levels, and represent the general statistical 
#' framework for evolutionary biology. Because of the progress in computational capacities mixed models 
#' have become increasingly popular among ecologists and evolutionary biologists over the last decade. 
#' However, running mixed model is not a straightforward exercise, and the way data are sampled among 
#' and within individuals can have strong implications on the outcome of the model. 
#' This is why we considered it was necessary to produce a simulation tool that could help new users 
#' interested in decomposing phenotypic variance to get more familiar with the concept of hierarchical 
#' organisation of traits, with mixed models and to avoid pitfalls caused by inappropriate sampling.
#' 
#' SQuID is made to help researchers to become familiar with multilevel variation, and to build up sampling designs for their study. 
#' SQuID is based on a mathematical model that creates a group of individuals (i.e. study population) 
#' repeatedly expressing phenotypes, for one or different traits, in uniform time. 
#' Phenotypic values of traits are generated following the general principle of the 
#' phenotypic equation (\href{http://onlinelibrary.wiley.com/doi/10.1111/1365-2656.12013/abstract}{Dingemanse & Dochtermann 2013, Journal of Animal Ecology}): 
#' phenotypic variance (Vp) is assumed to be the sum of a series of components. 
#' The user has thus the flexibility to add different variance components 
#' that will form the phenotype of the individual at each time step, and to set up the relative 
#' importance of each component. SQuiD then allows the user to collect a subsample of phenotypes 
#' for each simulated individual (i.e. operational data set), according to a specific sampling design. 
#' 
#' SQuID has two main functions; \code{\link{squidApp}} and \code{\link{squidR}}.
#' 
#' @section \code{\link{squidApp}} function:
#' This function runs the SQuID application which is a web-based platform created with the \href{http://shiny.rstudio.com/}{shiny} package. The SQuID is 
#' built up as a series of modules that guide the user into situations of increasing complexity 
#' to explore the dynamics between the way records are collected and estimates of parameters of specific interest; 
#' The last module is the full model simulation that allows the user to generate data sets that can then be used 
#' to run analyses in the statistical package of their choice for specific research questions. For most of the modules, 
#' the operational data set generated is automatically fed into a statistical model in R 
#' and the main results of the analysis shown in an output.
#' For the full model the user has the opportunity to download the operational data set for further analyses. The SQuID application also has a page (Full model (Step by step)) describing in details the SQuID full model.
#' 
#' @section \code{\link{squidR}} function:
#' This function is traditional R function that runs the SQuID full model. This function could be used for more advanced and efficient simulations once you understand how SQuID works.
#'
#' @docType package
#' @name SQUID
#' 
NULL