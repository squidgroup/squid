#################################################################
#################################################################
################# INCLUDE ALL PACKAGES ##########################
#################################################################
#################################################################

library(shiny)
library(shinyBS)
library(ggplot2)
library(lattice)
library(grid)
library(MASS)
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(arm))
suppressPackageStartupMessages(library(dplyr))

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
