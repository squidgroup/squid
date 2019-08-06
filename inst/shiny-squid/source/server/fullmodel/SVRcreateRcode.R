#Server functions for the full model
SVRcreateRcode <- function(input, myLabels) {

	output <- paste0("
##############################################################
########## R code to run a simulation with squidR() ##########
##############################################################

library(squid)


input <- list()


input$Tmax <- ",input[[myLabels$Tmax]]," # Time steps
input$NP   <- ",input[[myLabels$NP]]," # Number of replicates
input$NI   <- ",input[[myLabels$NI]]," # Number of individuals
input$NT   <- ",input[[myLabels$NT]]," # Number of traits
input$NG   <- ",input[[myLabels$NG]]," # Number of higher-level groups
	

#### Environment X1 ####

input$X1_state             <- ",input[[myLabels$X1_state]],"

	# Stochastic environmental effect
	input$X1_sto_state         <- ",input[[myLabels$X1_sto_state]],"
	input$X1_sto_shared        <- ",input[[myLabels$X1_sto_shared]]," # Stochastic environmental effect shared between individuals
	input$X1_sto_V             <- ",input[[myLabels$X1_sto_V]]," # Environmental effect variance
	input$X1_sto_autocor_state <- ",input[[myLabels$X1_sto_autocor_state]]," # Add autocorrelation to the stochastic environmental effect
	input$X1_sto_corr          <- ",input[[myLabels$X1_sto_corr]]," # Correlation value between two consecutive time steps
	
	# Linear environmental effect
	input$X1_lin_state         <- ",input[[myLabels$X1_lin_state]],"
	input$X1_lin_shared        <- ",input[[myLabels$X1_lin_shared]]," # Linear environmental effect shared between individuals
	input$X1_lin_intercept     <- ",input[[myLabels$X1_lin_intercept]]," # Linear intercept value
	input$X1_lin_slope         <- ",input[[myLabels$X1_lin_slope]]," # Linear slope value
	input$X1_lin_V             <- ",input[[myLabels$X1_lin_V]]," # Variance around mean environmental parameters
	
	# Cyclic environmental effect
	input$X1_cyc_state         <- ",input[[myLabels$X1_cyc_state]],"
	input$X1_cyc_shared        <- ",input[[myLabels$X1_cyc_shared]]," # Cyclic environmental effect shared between individuals
	input$X1_cyc_amplitude     <- ",input[[myLabels$X1_cyc_amplitude]]," # Amplitude value
	input$X1_cyc_period        <- ",input[[myLabels$X1_cyc_period]]," # Period value
	input$X1_cyc_Hshift        <- ",input[[myLabels$X1_cyc_Hshift]]," # Horizontal shift
	input$X1_cyc_Vshift        <- ",input[[myLabels$X1_cyc_Vshift]]," # Vertical shift
	input$X1_cyc_V             <- ",input[[myLabels$X1_cyc_V]]," # Variance around mean environmental parameters


#### Environment X2 ####

input$X2_state             <- ",input[[myLabels$X2_state]],"

	# Stochastic environmental effect
	input$X2_sto_state         <- ",input[[myLabels$X2_sto_state]],"
	input$X2_sto_shared        <- ",input[[myLabels$X2_sto_shared]]," # Stochastic environmental effect shared between individuals
	input$X2_sto_V             <- ",input[[myLabels$X2_sto_V]]," # Environmental effect variance
	input$X2_sto_autocor_state <- ",input[[myLabels$X2_sto_autocor_state]]," # Add autocorrelation to the stochastic environmental effect
	input$X2_sto_corr          <- ",input[[myLabels$X2_sto_corr]]," # Correlation value between two consecutive time steps
	
	# Linear environmental effect
	input$X2_lin_state         <- ",input[[myLabels$X2_lin_state]],"
	input$X2_lin_shared        <- ",input[[myLabels$X2_lin_shared]]," # Linear environmental effect shared between individuals
	input$X2_lin_intercept     <- ",input[[myLabels$X2_lin_intercept]]," # Linear intercept value
	input$X2_lin_slope         <- ",input[[myLabels$X2_lin_slope]]," # Linear slope value
	input$X2_lin_V             <- ",input[[myLabels$X2_lin_V]]," # Variance around mean environmental parameters
	
	# Cyclic environmental effect
	input$X2_cyc_state         <- ",input[[myLabels$X2_cyc_state]],"
	input$X2_cyc_shared        <- ",input[[myLabels$X2_cyc_shared]]," # Cyclic environmental effect shared between individuals
	input$X2_cyc_amplitude     <- ",input[[myLabels$X2_cyc_amplitude]]," # Amplitude value
	input$X2_cyc_period        <- ",input[[myLabels$X2_cyc_period]]," # Period value
	input$X2_cyc_Hshift        <- ",input[[myLabels$X2_cyc_Hshift]]," # Horizontal shift
	input$X2_cyc_Vshift        <- ",input[[myLabels$X2_cyc_Vshift]]," # Vertical shift
	input$X2_cyc_V             <- ",input[[myLabels$X2_cyc_V]]," # Variance around mean environmental parameters


# Interaction between environment X1 and X2.
input$X_Interaction        <- ",input[[myLabels$X_Interaction]],"

# Population mean values
input$B                    <- matrix(c(",paste(input[[myLabels$B]], collapse = ", "),"), 1, byrow = TRUE)
# Individual variance/correlation matrix
input$Vind                 <- matrix(c(",paste(input[[myLabels$Vind]], collapse = ", "),"), ",ncol(input[[myLabels$Vind]]),", byrow = FALSE)

# Residual variance
input$Ve                   <- matrix(c(",paste(input[[myLabels$Ve]], collapse = ", "),"), ",ifelse(is.null(ncol(input[[myLabels$Ve]])), 1, ncol(input[[myLabels$Ve]])),", byrow = FALSE)
# Higher-level grouping variance
input$VG                   <- matrix(c(",paste(input[[myLabels$VG]], collapse = ", "),"), ",ifelse(is.null(ncol(input[[myLabels$VG]])), 1, ncol(input[[myLabels$VG]])),", byrow = FALSE)

input$NR                   <- ",input[[myLabels$NR]]," # Mean number of records per individual
input$Vhsi                 <- ",input[[myLabels$Vhsi]]," # Among-individual variance in timing of sampling
input$NR_ind               <- ",input[[myLabels$NR_ind]]," # if TRUE, individuals will be sampled the same number of times
input$NR_trait             <- ",input[[myLabels$NR_trait]]," # if TRUE, traits within individuals will be sampled the same number of times
input$ST_ind               <- ",input[[myLabels$ST_ind]]," # if TRUE, individuals will be sampled at the same time (i.e. recorded simultaneously).
input$ST_trait             <- ",input[[myLabels$ST_trait]]," # if TRUE, traits within individuals will be sampled at the same times (i.e. recorded simultaneously).

# Run simulation
data <- squidR(input)


# Extract sampled data for analysis
sampled_data <- data$sampled_data

") 

  return(output)

}