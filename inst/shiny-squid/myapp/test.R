require(ggplot2)
require(grid)
# require(Rcpp)
# require(RcppArmadillo)
require("MASS")
# library(microbenchmark)


file.sources = paste("include/myInclude/fullModel/functions/",
                     list.files(path = "include/myInclude/fullModel/functions/", 
                                pattern = ".R"), 
                     sep = "")
S <- sapply(file.sources,source,.GlobalEnv)


# includePath     <- "include/myInclude/fullModel/functions/cpp/"
# file.sources    <- paste(includePath,list.files(includePath, pattern='.cpp'),sep="")
# 
# S <- sapply(file.sources,sourceCpp)


# cppDecayRate(c(1:100), 0.1, 100)



input <- list()

# input$test_B    <- matrix(1:8,1) 

# input$test_Vind        <- matrix(rep(0,64),8) 
# diag(input$test_Vind)  <- c(0.7,0.1,0.1,0.1,0.7,0.1,0.1,0.1)

# input$test_Vind <- matrix(c(0.7,rep(0,(4*4)-1)),4)

input$test_B    <- matrix(c(0,0.3,0,0),1)

input$test_Vind <- matrix(c(0.7 , 0  , 0 , 0,
                            0   , 0  , 0 , 0,
                            0   , 0  , 0 , 0,
                            0   , 0  , 0 , 0    
                          ), 
                          4)

input$test_Vme <- 0.05

input$test_Vk <- 0


# input$test_Vind <- matrix(c(0.7,  1  , 1   , 1   , 1   , 1   , 1   , 1  ,
#                            1   , 0.3 , 1   , 1   , 1   , 1   , 1   , 1  ,
#                            1   , 1   , 0   , 1   , 1   , 1   , 1   , 1  ,
#                            1   , 1   , 1   , 0   , 1   , 1   , 1   , 1  ,
#                            1   , 1   , 1   , 1   , 0.7 , 1   , 1   , 1  ,
#                            1   , 1   , 1   , 1   , 1   , 0.3 , 1   , 1  ,
#                            1   , 1   , 1   , 1   , 1   , 1   , 0   , 1  ,
#                            1   , 1   , 1   , 1   , 1   , 1   , 1   , 0  
#                        
# ), 
# 8)
# 
# 
# input$Vind <- matrix(c(0.7 ,  1 ,
#                        1   , 0.3 
# ), 
# 2)
  

input$test_X1_state      <- TRUE
input$test_X1_ran_state  <- TRUE
input$test_X1_ran_shared <- FALSE
input$test_X1_ran_autocorrelation <- FALSE
input$test_X1_ran_corr <- 0.85
input$test_X1_ran_V     <- 1


input$test_X1_lin_state <- FALSE
input$test_X1_lin_Slope         <- 0.1
# 
input$test_X1_cyc_state <- FALSE
# input$test_X1_cyc_Period <- 40
# 
# input$test_X2_state     <- TRUE
# input$test_X2_cyc_state <- TRUE
# 
# input$test_X_Interaction <- TRUE
 
input$test_Tmax <- 100
input$test_Time_sampling <- c(1,100)

input$test_NP <- 1
input$test_NI <- 100
input$test_NT <- 1
input$test_NR <- 10
input$test_NK <- 1

input$test_Drec_Ind    <- TRUE
input$test_Drec_Trait  <- TRUE
input$test_Dtime_Ind   <- FALSE
input$test_Dtime_Trait <- TRUE

input$test_Vit <- 0.5

environment <- NULL
session     <- NULL
progress    <- NULL


myModule <- "test"

data <- main(input, myModule, session, progress)

library(lme4)
library(arm)
library(dplyr)

df <- data$data_S
new_data <- df %>% 
              group_by(Individual) %>%
              summarise(Xmean = mean(X1)) %>%
              inner_join(df) %>%
              mutate(CMW = X1 - Xmean, 
                     CMB = Xmean - mean(X1))


LMR <- lmer(Phenotype ~ Xmean + (Xmean|Individual), data = data$data_S)
summary(LMR)


# print(data$myPlot$plotSampTime)
# ggsave(filename = "figures/sampling_vit_0.75.png",plot = data$myPlot$plotSampTime, scale = 0.8)



# Rprof(NULL)
# summaryRprof("file.out")
# 
# source("proftable.R")
# proftable("file.out")



# 
# library(compiler)
# g <- cmpfun(main)

# compare <- microbenchmark(,, times = 50)
# autoplot(compare)

# data <- list("myPlot"=myPlot, "data_S"=data_S, "data_C"=data_C, "V"=V)

# head(data$data_S)


print(multiplot(data$myPlot$plotX1,
                data$myPlot$plotX2,
                data$myPlot$plotX1X2,
                cols=1))

print(multiplot(data$myPlot$plotTotPhen,
                data$myPlot$plotSampPhen,
                data$myPlot$plotSampTime,
                cols=1))

# write.csv(x = data_S, file = "cyc_auto_sampled_data_Vit_0.csv")

# myPlot <- list("plotX1"=plot_X1, 
#                "plotX2"=plot_X2,
#                "plotEG"=plot_EG,
#                "plotES"=plot_ES,
#                "plotTotPhen"=plot_TotPhen, 
#                "plotSampPhen"=plot_SampPhen, 
#                "plotSampTime"=plot_SampTime)

# data$data_S$ME
# 
# densityplot(~data$data_S$ME, )  
# hist(data$data_S$ME,density = F)
# 
# 
# phen_time1   <- subset(data$data_S, data$data_S$time == data$data_S$time[1], select=Phenotype)
# phen_time2   <- subset(data$data_S, data$data_S$time == data$data_S$time[2], select=Phenotype)
# 
# plot(phen_time1$Phenotype~phen_time2$Phenotype)   




# 
# 
# 
# 
# hist(data$data_S$Phenotype)

# Vi        <- var(data$data_S$Phenotype)-var(data$data_S$ME)
# SE        <- sqrt(var(data$data_S$Phenotype)/length(data$data_S$Phenotype))
# mean      <- mean(data$data_S$Phenotype)
# LMR       <- lm(Phenotype ~ 1 , data = data$data_S)
# 
# resid(LMR)


library(lme4)
library(arm)
LMR <- lmer(Phenotype ~ X1 + (X1|Individual), data = data$data_S)

LMR <- lmer(Phenotype ~  0 + (1|Individual), data = data$data_S)
LMR <- update(LMR, ~.+ X1)

LMR <- lmer(Phenotype ~ -1  + (1|Individual), data = data$data_S)
LMR <- update(LMR, ~.+ X1 + (X1|Individual) - (1|Individual))
LMR <- update(LMR, ~.+ X2 + (X2|Individual) - (1|individual))




summary(LMR)
# 
# as.data.frame(VarCorr(LMR))
# 
# 
V <- as.data.frame(VarCorr(LMR))
P <- print(VarCorr(LMR),comp="Variance")


fixef(LMR) # get fixed effect coefficients
se.coef (LMR)
se.fixef(LMR) # get standard error of fixed effect coefficients

as.data.frame(VarCorr(LMR))$vcov # get random effect (variances)

re1 <- ranef(LMR, condVar=TRUE, whichel = "Individual") # get random effect for each individual
print(re1)
dotplot(re1) # plot random effect for each Individual with the standard error




# A function to extract simulated estimates of random effect paramaters from 
# lme4 objects using the sim function in arm
# whichel = the character for the name of the grouping effect to extract estimates for 
# nsims = the number of simulations to pass to sim
# x = model object
REsim <- function(x, whichel=NULL, nsims){
  require(plyr)
  mysim <- sim(x, n.sims = nsims)
  if(missing(whichel)){
    dat <- plyr::adply(mysim@ranef[[1]], c(2, 3), plyr::each(c(mean, median, sd)))
    warning("Only returning 1st random effect because whichel not specified")
  } else{
    dat <- plyr::adply(mysim@ranef[[whichel]], c(2, 3), plyr::each(c(mean, median, sd)))
  }
  return(dat)
}
REsim(LMR, whichel = "Individual", nsims = 1000)

# Dat = results of REsim
# scale = factor to multiply sd by
# var = character of "mean" or "median"
# sd = character of "sd"
plotREsim <- function(dat, scale, var, sd){
  require(eeptools)
  dat[, sd] <- dat[, sd] * scale
  dat[, "ymax"] <- dat[, var] + dat[, sd] 
  dat[, "ymin"] <- dat[, var] - dat[, sd] 
  dat[order(dat[, var]), "id"] <- c(1:nrow(dat))
  ggplot(dat, aes_string(x = "id", y = var, ymax = "ymax", 
                         ymin = "ymin")) + 
    geom_pointrange() + theme_dpi() + 
    labs(x = "Group", y = "Effect Range", title = "Effect Ranges") + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
          axis.text.x = element_blank(), axis.ticks.x = element_blank()) + 
    geom_hline(yintercept = 0, color = I("red"), size = I(1.1))
}

dat <- REsim(LMR, whichel = "Individual", nsims = 1000)
plotREsim(dat, scale = 1.2, var = "mean", sd = "sd")




#### Environments ######

timestep <- 10

# Random
  Vrandom <- 10
  
  ENVrandom <- rnorm(timestep, 0, sqrt(Vrandom))
  
  plot(ENVrandom)

# Autocorrelate

# Create decay rate matrix
  corr  <- 0.8
  alpha <- abs(log(corr))
  
  myMatrix <- matrix(0, nrow=timestep, ncol=timestep)
  myMatrix <- exp(-1*alpha*abs(col(myMatrix, as.factor = FALSE)-row(myMatrix, as.factor = FALSE)))
  
  ENVautocorrelate <-  myMatrix %*% ENVrandom
  
  plot(ENVautocorrelate)

# Linear
  a       <- 0
  b       <- 0.5
  Vlinear <- 1000
  
  ENVlinear <- rnorm(timestep, a + b * (1:timestep), sqrt(Vlinear))
  
  plot(ENVlinear)

# Cyclic
  
  Amplitude <- 10
  Period    <- 50
  Hshift    <- 10
  Vshift    <- 10
  
  A <- abs(Amplitude)       # |A| = the amplitude
  B <- (2*pi) / abs(Period) # 2pi/|B| = the period
  C <- -1 * Hshift * B      # -C/B = the phase shift (horizontal shift)f
  h <- Vshift               # vertical shift
  Vcyclic <- 0
  
  ENVcyclic <- rnorm(timestep, (A*sin(B*(1:timestep)+C)) + h, sqrt(Vcyclic))
  
  plot(ENVcyclic)




  NH <- 3
  NP <- 1
  NI <- 6
  NS <- 1
  NT <- 1
  
  
  H   <- rep(rep(rep(rnorm(NH*NP, 0, sqrt(1)), each=NI/NH), each=NS), NT)


  
  
  
  
# Function header example 
  
  myfunction <- function(){
    
    # Function description
    #
    # Args:
    #   arg1: arg1 description
    #   y: arg2 description.
    #
    # Returns:
    #   Return variable descriptions
    
    
    return()
    
  }






