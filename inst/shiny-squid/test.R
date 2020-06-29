library(ggplot2)
library(grid)
library(data.table)

input <- list()

input$Tmax          <- 100 # default = 1
input$Time_sampling <- c(1,100)

input$NP <- 1 # default = 1
input$NI <- 100# default = 1
input$NT <- 1 # default = 1
input$NG <- 1 # default = 1
input$NR <- 20 # default = 1

# input$B    <- rep(0, 8)
input$B    <- rep(c(0 , 0, 0, 1),input$NT)

# (Co)Variance matrix
input$Vind <- matrix(0, 4, 4)
# Variances
input$Vind[1,1] <- 0.7
input$Vind[2,2] <- 0.4
# Correlations
input$Vind[2,1] <- 0.5

# Residual (Co)Variance matrix
input$Ve <- matrix(0, input$NT, input$NT)
diag(input$Ve) <- 0.05
# input$Ve[2,1]  <- -0.5


# Envrionmental covariates
input$X1_state      <- TRUE # default = FALSE
# Stochastic
input$X1_sto_state  <- TRUE  # default = FALSE
input$X1_sto_shared <- FALSE # default = TRUE


input$X2_state      <- TRUE # default = FALSE
# Stochastic
input$X2_sto_state  <- TRUE  # default = FALSE
input$X2_sto_shared <- FALSE # default = TRUE

input$X_Interaction <- TRUE

mydata <- as.data.table(squid::squidR(input = input, plot = FALSE)$sampled_data)



#### lmer ######

LMR      <- lme4::lmer(Phenotype ~ 1 + X1*X2 + (1|Individual), data = mydata)

FIXEF    <- lme4::fixef(LMR)
SE.FIXEF <- arm::se.fixef(LMR)
RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov



#### 3d plots ####

X_seq <- seq(from = min(mydata[ , c("X1", "X2")]), to = max(mydata[ , c("X1", "X2")]), length.out = 10)

predictors      <- cbind("intecept" = 1, expand.grid("X1" = X_seq, "X2" = X_seq))
predictors$X1X2 <- predictors$X1 * predictors$X2


Phenotype_mean <- as.matrix(predictors) %*% as.vector(input$B)
Phenotype_mean <- t(matrix(Phenotype_mean, nrow = length(X_seq), ncol = length(X_seq)))


library(plotly)
plot_ly(hoverinfo = "none")  %>%
  add_surface(x = X_seq, y = X_seq, z = Phenotype_mean, opacity = 0.7,
              colorscale = list(c(0, 1), c("black", "black"))) %>%
  add_markers(data = mydata, x = ~X1, y = ~X2, z = ~Phenotype, color = ~Individual, size = 3) %>%
  add_markers(data = mydata, x = 0, y = 0, z = input$B[1], color = c("black"), size = 20) %>%
  layout(showlegend = FALSE) %>%
  hide_colorbar()




 
 
##### Multivariate models ####

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
