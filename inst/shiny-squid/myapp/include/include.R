#################################################################
#################################################################
##################### INCLUDE PACKAGES ##########################
#################################################################
#################################################################

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyBS))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(lattice))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(lme4))
suppressPackageStartupMessages(library(arm))
suppressPackageStartupMessages(library(dplyr))

#################################################################
#################################################################
################### SOURCE Functions ############################
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
