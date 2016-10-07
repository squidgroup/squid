

```r
# Load squid package
library(squid)

################
# SQuID inputs #
################

# Prepare squid input data.frame
parameters <- data.frame( "NI"        = c(50 , 100), # Number of individuals
                          "NR"        = c(10, 5),    # Number of samples per individual
                          "VI"        = 0.5,         # Among-individual variance (intercept)
                          "B0"        = 0.5,         # Population mean phenotype (intercept)
                          "Ve"        = 0.5,         # Measurement error variance
                          "Tmax"      = 100         # total time steps
                          )

#######################
# Run all simulations #
#######################

dt <- NULL

for (i in 1:nrow(parameters)) { 
	
  inputs <- as.list(parameters[i,])
  # Prepare variance/correlation matrix
  inputs$Vind      <- matrix(0, nrow = 4, ncol = 4)
  inputs$Vind[1,1] <- parameters$VI[i]
  
  # Prepare population mean value matrix (1 row)
  inputs$B         <- c(parameters$B0[i], rep(0,3))
  
  # Run simulation
  dt_tmp <- squid::squidR(inputs)$sampled_data
  dt_tmp <- cbind(dt_tmp, parameters[i, c("NI", "NR")])
  
  # Compile present simulation to the total simulation table
  dt <- rbind(dt, dt_tmp)
}


# Example of subsetting data 
sub_data <- subset(dt, NI == 100 & NR == 3)
```

