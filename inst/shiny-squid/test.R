require(ggplot2)
require(grid)

input <- list()

input$B    <- matrix(c(0,0.3,0,0),1)
input$Vind <- matrix(c(0.7 , 0  , 0 , 0,
                            1   , 0.5  , 0 , 0,
                            0   , 0  , 0 , 0,
                            0   , 0  , 0 , 0    
                          ), 
                          4)

input$Ve <- 0.05
input$VG <- 0


# input$Vind <- matrix(c(0.7,  1  , 1   , 1   , 1   , 1   , 1   , 1  ,
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
  
# Environment X1
  input$X1_state      <- TRUE
  # Stochastic
    input$X1_sto_state  <- TRUE  # default = FALSE
    input$X1_sto_shared <- TRUE  # default = TRUE
    input$X1_sto_V      <- 1     # default = 1
  # Autocorrelate
    input$X1_sto_autocor_state   <- TRUE # default = FALSE
    input$X1_sto_corr            <- 0.9     # default = 0
  # Linear
    input$X1_lin_state      <- FALSE  # default = FALSE
    input$X1_lin_shared     <- TRUE   # default = TRUE
    input$X1_lin_intercept  <- 0      # default = 0
    input$X1_lin_slope      <- 1      # default = 1
    input$X1_lin_V          <- 0      # default = 1
  # Cyclic
    input$X1_cyc_state     <- FALSE # default = FALSE
    input$X1_cyc_shared    <- TRUE  # default = TRUE
    input$X1_cyc_amplitude <- 10    # default = 10
    input$X1_cyc_period    <- 10    # default = 10 
    input$X1_cyc_Hshift    <- 0     # default = 0
    input$X1_cyc_Vshift    <- 0     # default = 0

# Environment X2
    input$X2_state      <- TRUE
    # Stochastic
    input$X2_sto_state  <- TRUE  # default = FALSE
    input$X2_sto_shared <- TRUE  # default = TRUE
    input$X2_sto_V      <- 1     # default = 1
    # Autocorrelate
    input$X2_sto_autocor_state   <- TRUE # default = FALSE
    input$X2_sto_corr            <- 0.9     # default = 0
    # Linear
    input$X2_lin_state      <- TRUE  # default = FALSE
    input$X2_lin_shared     <- TRUE   # default = TRUE
    input$X2_lin_intercept  <- 0      # default = 0
    input$X2_lin_slope      <- 1      # default = 1
    input$X2_lin_V          <- 0      # default = 1
    # Cyclic
    input$X2_cyc_state     <- TRUE # default = FALSE
    input$X2_cyc_shared    <- TRUE  # default = TRUE
    input$X2_cyc_amplitude <- 10    # default = 10
    input$X2_cyc_period    <- 10    # default = 10 
    input$X2_cyc_Hshift    <- 0     # default = 0
    input$X2_cyc_Vshift    <- 0     # default = 0

input$X_Interaction <- TRUE # default = FALSE
    
input$Tmax <- 100 # default = 1
input$Time_sampling <- c(1,100)

input$NP <- 1 # default = 1
input$NI <- 5 # default = 1
input$NT <- 1 # default = 1
input$NR <- 5 # default = 1
input$NG <- 1 # default = 1

input$NR_ind   <- TRUE # default = TRUE
input$NR_trait <- TRUE # default = TRUE
input$ST_ind   <- TRUE # default = TRUE
input$ST_trait <- TRUE # default = TRUE

input$Visj <- 0

data <- SQUID::runSQUIDfct(input=input, plot=TRUE)


print(SQUID::multiplot(data$myPlot$plotX1,
                data$myPlot$plotX2,
                data$myPlot$plotX1X2,
                cols=1))

print(SQUID::multiplot(data$myPlot$plotTotPhen,
                data$myPlot$plotSampPhen,
                data$myPlot$plotSampTime,
                cols=1))

#--------------------------------------------------------------------------------------------

LMR <- lmer(Phenotype ~ 1 + X1 + (X1|Individual), data = test)
summary(LMR)

cov2cor(VarCorr(LMR)$Individual[,])[2]



ggplot(data = data$data_S, aes(y=Phenotype, x=X1, color=as.factor(Individual))) +
  stat_smooth(method = "lm", se=FALSE) + 
  theme(legend.position="none") + 
  xlab("Environmental effect") + 
  ylab("Phenotype")




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




library(lme4)
library(arm)
LMR <- lmer(Phenotype ~ 0 + X1 + (X1|Individual), data = data$data_S)
summary(LMR)

LMR <- lmer(Phenotype ~  0 + (1|Individual), data = data$data_S)
LMR <- update(LMR, ~.+ X1)

LMR <- lmer(Phenotype ~ -1  + (1|Individual), data = data$data_S)
LMR <- update(LMR, ~.+ X1 + (X1|Individual) - (1|Individual))
LMR <- update(LMR, ~.+ X2 + (X2|Individual) - (1|individual))

LMR      <- lme4::lmer(Phenotype ~ 1 + X1 + (1|Individual) + (0+X1|Individual), data = data$data_S)
RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov

summary(LMR)
V <- as.data.frame(VarCorr(LMR))
P <- print(VarCorr(LMR),comp="Variance")


fixef(LMR) # get fixed effect coefficients
se.coef (LMR)
se.fixef(LMR) # get standard error of fixed effect coefficients

as.data.frame(VarCorr(LMR))$vcov # get random effect (variances)

re1 <- ranef(LMR, condVar=TRUE, whichel = "Individual") # get random effect for each individual
print(re1)
dotplot(re1) # plot random effect for each Individual with the standard error
