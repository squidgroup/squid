#Server functions for the full model
SVRcreateRcode <- function(input, myLabels) {
  
	
	output <- NULL
	
	output <- paste0("
##############################################################
########### R code to run simulation with squidR() ###########
##############################################################


input <- list()


input$Tmax <- ",input[[myLabels$Tmax]],"      # Time steps
input$NP <- ",input[[myLabels$NP]],"        # Number of replicates
input$NI <- ",input[[myLabels$NI]],"        # Number of individuals
input$NT <- ",input[[myLabels$NT]],"        # Number of traits
input$NG <- ",input[[myLabels$NG]],"        # Number of higher-level groups
	
X1_state
X1_sto_state 
X1_sto_shared 
X1_sto_V 
X1_sto_autocor_state
X1_sto_corr
X1_lin_state
X1_lin_shared
X1_lin_intercept
X1_lin_slope
X1_lin_V
X1_cyc_state
X1_lin_shared 
X1_lin_intercept
X1_lin_slope
X1_lin_V
X1_cyc_state
X1_cyc_shared
X1_cyc_amplitude
X1_cyc_period
X1_cyc_Hshift
X1_cyc_Vshift
X1_cyc_V
X2_state
X2_sto_state
X2_sto_shared
X2_sto_V
X2_sto_autocor_state
X2_sto_corr
X2_lin_state
X2_lin_shared
X2_lin_intercept
X2_lin_slope
X2_lin_V
X2_cyc_state
X2_cyc_shared
X2_cyc_amplitude
X2_cyc_period
X2_cyc_Hshift
X2_cyc_Vshift
X2_cyc_V
X_Interaction
B 
Vind
Ve
VG
NR
Vhsi
NR_ind
NR_trait
ST_ind
ST_trait

end
") 

  return(output)
  
}