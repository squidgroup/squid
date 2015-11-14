#################################################################
#################################################################
################# INCLUDE ALL PACKAGES ##########################
#################################################################
#################################################################

# Install packages if not found
required.packages <- c("shiny", "shinyBS","ggplot2", "lattice","grid", "MASS", "lme4", "arm")
new.packages <- required.packages[!(required.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

require(shiny)
require(shinyBS)
require(ggplot2)
require(lattice)
require(grid)
require(MASS)
# require(Rcpp)
# require(RcppArmadillo)
require(lme4)
require(arm)

#################################################################
#################################################################
################ SOURCE ALL Functions ###########################
#################################################################
#################################################################

includePath     <- "include/myInclude/"
file.sources    <- paste(includePath,list.files(includePath,
                                                recursive=TRUE,
                                                pattern='*.R$'),sep="")

source(paste(includePath,"variables/general/VARgeneral.R",sep=""))
S <- sapply(file.sources,source,.GlobalEnv)


# includePath     <- "include/myInclude/fullModel/functions/cpp/"
# file.sources    <- paste(includePath,list.files(includePath, pattern='*.cpp$'),sep="")
# 
# S <- sapply(file.sources,sourceCpp)
