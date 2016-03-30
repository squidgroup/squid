#' @title run SQuID application 
#'
#' @description \code{squidApp} runs SQuID application as a web interface that has been developed with the package \href{http://shiny.rstudio.com/}{shiny}.
#' 
#' SQuID is made to help researchers to become familiar with multilevel variation, and to build up sampling designs for their study. SQuID is built up as a series of modules that guide the user into situations of increasing complexity to explore the dynamics between the way records are collected and estimates of parameters of specific interest; The last module is the full model simulation package that allows the user to generate data sets that can then be used to run analyses in the statistical package of their choice for specific research questions.
#' 
#' SQuID is based on a mathematical model that creates a group of individuals (i.e. study population) repeatedly expressing phenotypes, for one or different traits, in uniform time. Phenotypic values of traits are generated following the general principle of the phenotypic equation (\href{http://onlinelibrary.wiley.com/doi/10.1111/1365-2656.12013/abstract}{Dingemanse & Dochtermann 2013, Journal of Animal Ecology}): phenotypic variance (Vp) is assumed to be the sum of a series of components (see the full model). The user has thus the flexibility to add different variance components that will form the phenotype of the individual at each time step, and to set up the relative importance of each component. SQuID then allows the user to collect a subsample of phenotypes for each simulated individual (i.e. operational data set), according to a specific sampling design. For most of the modules, the operational data set generated is automatically fed into a statistical model in R and the main results of the analysis shown in an output. For the full model the user has the opportunity to download the operational data set for further analyses.
#'
#' @param launch.browser        \code{logical}; If \code{TRUE} (default), the system's default web browser will be launched automatically after the app is started. If \code{FALSE} the app will be launched in an interactive session only. The value of this parameter can also be a function to call with the application's URL.
#' @param ...                   any argument that could be passed to the function \code{\link[shiny]{runApp}} from the \pkg{shiny} package.
#'
#' @details
#' SQuID application will by default show up on your default web browser as a web site. In order to use properly the SQuID application, we encourage you to start by reading the documentation on the portal page. From there you will be guided through SQuID application according to your experience and knowledge of multilevel modelling.    
#' 
#' For more advanced and efficient simulations, SQuID is also available as an R function \code{\link{squidR}}.
#' 
#' @return NULL
#' 
#' @seealso \code{\link{squidR}}
#' 
#' @examples 
#' squidApp()
#' 
#' @import dplyr
#' @import data.table
#' @export
#' 
squidApp <- function(launch.browser=TRUE, ...) {
  
  appDir <- system.file("shiny-squid", package = "SQUID")
  
  if (appDir == "") stop("Could not find SQuID application directory. Try re-installing `SQUID` package.", call. = FALSE)
  
  shiny::runApp(appDir=appDir, launch.browser=launch.browser, display.mode="normal", ...)
}