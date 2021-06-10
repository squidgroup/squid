library(ggplot2)
library(grid)
library(data.table)

input <- list()

input$Tmax          <- 100 # default = 1
input$Time_sampling <- c(1,100)

input$NP <- 1 # default = 1
input$NI <- 200# default = 1
input$NT <- 1 # default = 1
input$NG <- 1 # default = 1
input$NR <- 5 # default = 1

# input$B    <- rep(0, 8)
input$B    <- rep(c(0.5, 0, 0, 0),input$NT)

# (Co)Variance matrix
input$Vind <- matrix(0, 4, 4)
# Variances
input$Vind[1,1] <- 0.7
input$Vind[2,2] <- 0
input$Vind[3,3] <- 0
input$Vind[4,4] <- 0
# Correlations
input$Vind[2,1] <- 0
input$Vind[3,1] <- 0
input$Vind[3,2] <- 0
input$Vind[4,1] <- 0
input$Vind[4,2] <- 0
input$Vind[4,3] <- 0

# Residual (Co)Variance matrix
input$Ve <- matrix(0, input$NT, input$NT)
diag(input$Ve) <- 0.1
# input$Ve[2,1]  <- -0.5


# Envrionmental covariates
input$X1_state      <- FALSE # default = FALSE
# Stochastic
input$X1_sto_state  <- TRUE  # default = FALSE
input$X1_sto_shared <- TRUE # default = TRUE


input$X2_state      <- FALSE # default = FALSE
# Stochastic
input$X2_sto_state  <- TRUE  # default = FALSE
input$X2_sto_shared <- TRUE # default = TRUE

input$X_Interaction <- FALSE

mydata <- as.data.table(squid::squidR(input = input, plot = FALSE)$sampled_data)


#### glmer ######

mydata$Phenotype_latent <- rpois(nrow(mydata), exp(mydata$Phenotype))
mydata$Overdispersion   <- as.factor(1:nrow(mydata))

fit <- lme4::glmer(Phenotype_latent ~ 1 + (1|Individual) + (1|Overdispersion), 
                   family = poisson(link="log"), 
                   data   = mydata)
summary(fit)

FIXEF    <- lme4::fixef(fit)
SE.FIXEF <- arm::se.fixef(fit)
RANDEF   <- as.data.frame(lme4::VarCorr(fit))$vcov


#### lmer ######

LMR      <- lme4::lmer(Phenotype ~ 1 + X1*X2 + (1+X1*X2|Individual), data = mydata)

FIXEF    <- lme4::fixef(LMR)
SE.FIXEF <- arm::se.fixef(LMR)
RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov



#### 3d plots ####

X_seq <- seq(from = min(mydata[ , c("X1", "X2")]), to = max(mydata[ , c("X1", "X2")]), length.out = 10)

predictors      <- cbind("intecept" = 1, expand.grid("X1" = X_seq, "X2" = X_seq))
predictors$X1X2 <- predictors$X1 * predictors$X2


Phenotype_mean <- as.matrix(predictors) %*% as.vector(input$B)
Phenotype_mean <- t(matrix(Phenotype_mean, nrow = length(X_seq), ncol = length(X_seq)))


newdata     <- copy(mydata)
newdata     <- newdata[Individual %in% sample(unique(Individual),3)]

Ind_data <- lapply(unique(newdata$Individual), function(id){
  
  id <- unique(newdata$Individual)[1]

  dt <- copy(newdata[Individual == id])
  
  X1_seq <- seq(min(dt$X1), max(dt$X2), length.out = 10)
  X2_seq <- seq(min(dt$X2), max(dt$X2), length.out = 10)
  
  X     <- cbind("intecept" = 1,
                  expand.grid("X1" = X1_seq, 
                              "X2" = X2_seq))
  X$X1X2 <- X$X1 * X$X2
  
  blup <- as.numeric(lme4::ranef(LMR)$Individual[id, ])
  
  Phenotype <- as.matrix(X) %*% (as.vector(input$B) + blup)
  Phenotype <- t(matrix(Phenotype, nrow = length(X1_seq), ncol = length(X2_seq)))
  
  return(list("X1_seq"    = X1_seq,
              "X2_seq"    = X2_seq, 
              "Phenotype" = Phenotype))
  
})


plotly::plot_ly(hoverinfo = "none")  %>%
  plotly::add_surface(x = Ind_data[[1]]$X1_seq, y = Ind_data[[1]]$X2_seq, z = Ind_data[[1]]$Phenotype,
                      opacity = 0.7, colorscale = list(c(0, 1), c("yellow", "yellow"))) %>%
  plotly::add_surface(x = Ind_data[[2]]$X1_seq, y = Ind_data[[2]]$X2_seq, z = Ind_data[[2]]$Phenotype,
                      opacity = 0.7, colorscale = list(c(0, 1), c("blue", "blue"))) %>%
  plotly::add_surface(x = Ind_data[[3]]$X1_seq, y = Ind_data[[3]]$X2_seq, z = Ind_data[[3]]$Phenotype,
                      opacity = 0.7, colorscale = list(c(0, 1), c("green", "green"))) %>%
  
  plotly::add_surface(x = X_seq, y = X_seq, z = Phenotype_mean, opacity = 0.7,
                      colorscale = list(c(0, 1), c("black", "black"))) %>%
  plotly::layout(showlegend = FALSE) %>%
  plotly::hide_colorbar() %>%
  plotly::layout(scene = list(xaxis=list(title = "X1"),
                              yaxis=list(title = "X2"),  
                              zaxis=list(title = "Phenotype")))




 
 
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
