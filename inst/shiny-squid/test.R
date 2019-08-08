library(ggplot2)
library(grid)
library(data.table)

input <- list()

input$Tmax          <- 100 # default = 1
input$Time_sampling <- c(1,100)

input$NP <- 1 # default = 1
input$NI <- 100# default = 1
input$NT <- 2 # default = 1
input$NG <- 1 # default = 1
input$NR <- 10 # default = 1

# input$B    <- rep(0, 8)
input$B    <- rep(c(0 , 0.2, 0, 0),2)

# (Co)Variance matrix
input$Vind <- matrix(0, 8, 8)
# Variances
input$Vind[1,1] <- 0.5
input$Vind[5,5] <- 0.5
# Correlations
input$Vind[5,1] <- 0.5

# Residual (Co)Variance matrix
input$Ve <- matrix(0, input$NT, input$NT)
diag(input$Ve) <- 0.05
input$Ve[2,1]  <- -0.5


# Envrionmental covariates
input$X1_state      <- TRUE # default = FALSE
# Stochastic
input$X1_sto_state  <- TRUE  # default = FALSE

# input$X2_state      <- TRUE # default = FALSE
# # Stochastic
# input$X2_sto_state  <- TRUE  # default = FALSE

# input$X_Interaction <- FALSE

mydata <- as.data.table(squid::squidR(input = input, plot = FALSE)$sampled_data)


mydata <- mydata[ , .(Time, Individual, Trait, Phenotype, X1)]
mydata[ , Trait := paste0("Phenotype_", Trait)]
mydata <- dcast(mydata, Time + Individual + X1 ~ Trait, value.var = "Phenotype")




library(brms)


fit <- brm(mvbind(Phenotype_1, Phenotype_2) ~ 1 + (1|p|Individual),
                  data  = mydata, iter = 10, chains = 1)


saveRDS(fit, "./inst/shiny-squid/source/server/modules/module4/stanFiles/brms_model_module4_step5_1.rds")


fit1 <- readRDS("./inst/shiny-squid/source/server/modules/module4/stanFiles/brms_model_module4_step5_2.rds")
fit2 <- update(fit1, newdata = mydata,
               iter = 500, warmup = 100, chains = 1, cores = 8)


summary(fit2)

RanCoef <- VarCorr(fit2)
FixCoef <- fixef(fit2, summary = TRUE)

# Random effect

# Individual effect
RanCoef[["Individual"]][["sd"]]["Phenotype1_Intercept", "Estimate"]^2
RanCoef[["Individual"]][["sd"]]["Phenotype2_Intercept", "Estimate"]^2
RanCoef[["Individual"]][["cov"]]["Phenotype1_Intercept", "Estimate", "Phenotype2_Intercept"]
RanCoef[["Individual"]][["cor"]]["Phenotype1_Intercept", "Estimate", "Phenotype2_Intercept"]


# Residual effect
RanCoef[["residual__"]][["sd"]]["Phenotype1", "Estimate"]^2
RanCoef[["residual__"]][["sd"]]["Phenotype2", "Estimate"]^2
RanCoef[["residual__"]][["cov"]]["Phenotype1", "Estimate", "Phenotype2"]
RanCoef[["residual__"]][["cor"]]["Phenotype1", "Estimate", "Phenotype2"]



# Fixed effect
FixCoef["Phenotype1_Intercept", "Estimate"]
FixCoef["Phenotype2_Intercept", "Estimate"]
FixCoef["Phenotype1_X1", "Estimate"]
FixCoef["Phenotype2_X1", "Estimate"]














# ##### MULTIPLOTS #######
# 
# print(multiplot(data$plot$X1,
#                 data$plot$X2,
#                 data$plot$X1X2,
#                 cols=1))
# 
# print(multiplot(data$plot$totPhen,
#                 data$plot$sampPhen,
#                 data$plot$sampTime,
#                 cols=1))



LMR  <- lme4::lmer(Phenotype ~ X1 + (1|Individual), data = data$sampled_data)

FIXEF    <- lme4::fixef(LMR)
SE.FIXEF <- arm::se.fixef(LMR)
RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov

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
