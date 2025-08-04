---
output: 
  html_document: 
    keep_md: true
---

## Run multiple simulations with `squidR()`

SQuID was designed to provide a user-friendly and web-based program to simulate data for testing a variety of ideas about sampling and bias in hierarchical mixed modelling. For those very familiar with these approaches and curious about SQuID, you might be interested in using the SQuID R function `squidR()` that is also used by `squidApp()` to simulate data. However, before getting into simulating data with `squidR()`, we  recommend that you familiarize yourself with the various SQuID phenotypic equation components by reading the documentation on the *Full model (Step by step)* page within the SQuID app. 

The advantages of using the SQuID R function instead of the SQuID app are that:

1. you can incorporate the SQuID R function into your own R code.
2. you have more flexibility in how you specify the input parameters. For instance, you could simulate distinct data sets with a range of values for some parameters.

Below we present an example of using the function `squidR()`. In this example, we simulate 2 different data sets where in the first scenario we sample 50 individuals 10 times and in the second scenario we sample 100 individuals 5 times. 

First, we load the `squid` package.


```r
library(squid)
```

Then, we prepare the input parameters for the 2 distinct simulation scenarios. We store these values in a `data.frame` where each row represents a new simulation scenario. It is important that the column names are equivalent to the parameter names used by `squidR()`. All the parameter names are listed and described in the `squidR()` R documentation which is accessible by running the help command (i.e. `?squid()`). An exception to that is when you have to specify a matrix input parameter (and not a scalar input) such as the variance/correlation matrix (e.g. `Vind`). In this case, you have to define the value of each element in the matrix separately (i.e. distinct columns). At this stage, the names assigned to the matrix elements are arbitrary and will not be inputted into `squidR()`. We will see below how we reconstruct the input matrix before running `squidR()`. 



```r
################
# SQuID inputs #
################

# Prepare squid input data.frame
parameters <- data.frame( "NI"        = c(50 , 100), # Number of individuals
                          "NR"        = c(10, 5),    # Number of samples per individual
                          "VI"        = 0.5,         # Among-individual variance (intercept)
                          "B0"        = 0.5,         # Population mean phenotype (intercept)
                          "Ve"        = 0.5,         # Measurement error variance
                          "Tmax"      = 100          # total time steps
                          )

parameters
```

```
##    NI NR  VI  B0  Ve Tmax
## 1  50 10 0.5 0.5 0.5  100
## 2 100  5 0.5 0.5 0.5  100
```


Finally, we run the different simulation scenarios one by one by looping `squidR()` function which simulates the data for each row of the parameter `data.frame`. We start by converting the parameter row into a `list` which is expected by `squidR()`. We then reconstruct the variance/correlation matrix (4x4) by assigning the variances and correlations at the right position. In this example, we just assign the value of the among-individual variance at the intercept. At this stage the name of the matrix (i.e. `Vind`) has to be the one expected by `squidR()`. Similarly, we reconstruct the population mean matrix `B`. When the input parameters are defined, we run the current simulation and combine this generated data with the total data table (using the function `rbind()`). Note that we only recover the sampled data and that we also add the simulation parameters to be able to recover the data associated to each simulation scenario.


```r
#######################
# Run all simulations #
#######################

run_sim <- function(i){
  
  # Select simulation parameters
  param            <- parameters[i, ]
  
  # Converting the parameters data.frame into a list
  inputs           <- as.list(param)
  
  # Create variance/correlation matrix
  inputs$Vind      <- matrix(0, nrow = 4, ncol = 4)
  inputs$Vind[1,1] <- param$VI

  # Create population mean value matrix (1 row)
  inputs$B         <- c(param$B0, rep(0,3))

  # Run simulation
  dt               <- squid::squidR(inputs)$sampled_data

  # Add parameter values to data.frame
  return(cbind(dt, param))
}

dt_sim <- do.call("rbind", lapply(1:nrow(parameters), run_sim))
```

Now you can analyse you simulated data and investigate, for instance, how different sampling designs will impact you statistical parameter estimation. You can extract the data from a specific scenario using the function `subset()` as described below: 


```r
# Example of subsetting data 
sub_data <- subset(dt_sim, NI == 100 & NR == 5)
```
