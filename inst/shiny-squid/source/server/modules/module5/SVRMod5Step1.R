#Server functions for module 5 step 1
c(
    ######### Set variables ######### 
    Mod5Step1updateVind <- function(input, nb.IS){
    	df <- matrix(rep(0,nb.IS*nb.IS),nb.IS)
    	diag(df)[1] <- input$Mod5Step1_Vi
    	return(as.data.frame(df))
    },
    # Set hidden variables
     output$Mod5Step1_hidden <- renderUI({
        list(
          numericInput("Mod5Step1_Tmax", "", Modules_VAR$Tmax$max),
          matrixInput2("Mod5Step1_Vind", "", Mod5Step1updateVind(input, nb.IS)),
          matrixInput2("Mod5Step1_B",    "", data.frame(matrix(c(0,input$Mod5Step1_B1,input$Mod5Step1_B2,0),1))),
          checkboxInput("Mod5Step1_X1_state", "", value = TRUE),
          checkboxInput("Mod5Step1_X1_sto_state", "", value = TRUE),
          checkboxInput("Mod5Step1_X2_state", "", value = TRUE),
          checkboxInput("Mod5Step1_X2_sto_state", "", value = TRUE)
        )
     }),
    outputOptions(output, "Mod5Step1_hidden", suspendWhenHidden = FALSE),
    
   ######### Run simulation #########
   # Run simulation and return results
   Mod5Step1_output <- reactive({
   	
   	if (input$Mod5Step1_Run == 0) # if Run button is pressed
   		   return(NULL)
   	
   	isolate({ 
   		
   		updateCheckboxInput(session, "isRunning", value = TRUE)
   		
   		# Call app main function
   		data <- squid::squidR(input, module = "Mod5Step1")  
   		
   		LMR      <- lme4::lmer(Phenotype ~ 1 + X1 + (1|Individual), data = data$sampled_data)
   		FIXEF    <- lme4::fixef(LMR)
   		SE.FIXEF <- arm::se.fixef(LMR)
   		RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
   		
   		data$Vi       <- round(RANDEF[1],2)
   		data$Vr       <- round(RANDEF[2],2) 
   		
   		data$B1      <- round(FIXEF["X1"],2)
   		data$se.B1   <- round(SE.FIXEF["X1"],2)
   		
   		updateCheckboxInput(session, "isRunning", value = FALSE)
   		
   		return(data)
   	})  
   }),
   
   Mod5Step1_output2 <- reactive({
   	
   	if (input$Mod5Step1_Run == 0) # if Run button is pressed
   		return(NULL)
   	
   	isolate({ 
   		
   		updateCheckboxInput(session, "isRunning", value = TRUE)
   		
   		# Call app main function
   		data <- squid::squidR(input, module = "Mod5Step1")  
   		
   		LMR      <- lme4::lmer(Phenotype ~ 1 + X1 + X2 + (1|Individual), data = data$sampled_data)
   		FIXEF    <- lme4::fixef(LMR)
   		SE.FIXEF <- arm::se.fixef(LMR)
   		RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
   		
   		data$Vi       <- round(RANDEF[1],2)
   		data$Vr       <- round(RANDEF[2],2) 
   		
   		data$B1      <- round(FIXEF["X1"],2)
   		data$se.B1   <- round(SE.FIXEF["X1"],2)
   		
   		data$B2      <- round(FIXEF["X2"],2)
   		data$se.B2   <- round(SE.FIXEF["X2"],2)
   		
   		updateCheckboxInput(session, "isRunning", value = FALSE)
   		
   		return(data)
   	})  
   }),
   
   Mod5Step1_table1 <- reactive({
   	
   	data    <- Mod5Step1_output()
   	
   	myTable <- data.frame(
   		"True"       = c(paste0("$",EQ3$mean1,"$ = ",input$Mod5Step1_B[2]),
   										 paste0("Individual variance ($V_",NOT$devI,"$) = ",input$Mod5Step1_Vi),
   										 paste0("Residual variance ($V_",NOT$residualUpper,"$) = ",input$Mod5Step1_Ve)),
   		"Estimated"  = c(paste0("$",EQ3$mean1,"$ = ",ifelse(!is.null(data),paste(data$B1,"\U00b1", data$se.B1),"...")),
   										 paste0("Individual variance ($V_",NOT$devI,"$) = ", ifelse(!is.null(data),data$Vi,"...")),
   										 paste0("Residual variance of sample ($V'_",NOT$residualUpper,"$) = ", ifelse(!is.null(data),data$Vr,"..."))))  
   	
   	return(getTable(myTable))
   }),
   
   # Display results (table)
   output$Mod5Step1_summary_table1 <- renderUI({Mod5Step1_table1()}),
   
   Mod5Step1_table2 <- reactive({
   	
   	data    <- Mod5Step1_output2()
   	
   	myTable <- data.frame(
   		"True"       = c(paste0("$",EQ3$mean1,"$ = ",input$Mod5Step1_B[2]),
   										 paste0("$",EQ3$mean2,"$ = ",input$Mod5Step1_B[3]),
   										 paste0("Individual variance ($V_",NOT$devI,"$) = ",input$Mod5Step1_Vi),
   										 paste0("Residual variance ($V_",NOT$residualUpper,"$) = ",input$Mod5Step1_Ve)),
   		"Estimated"  = c(paste0("$",EQ3$mean1,"$ = ",ifelse(!is.null(data),paste(data$B1,"\U00b1", data$se.B1),"...")),
   										 paste0("$",EQ3$mean2,"$ = ",ifelse(!is.null(data),paste(data$B2,"\U00b1", data$se.B2),"...")),
   										 paste0("Individual variance ($V_",NOT$devI,"$) = ", ifelse(!is.null(data),data$Vi,"...")),
   										 paste0("Residual variance of sample ($V'_",NOT$residualUpper,"$) = ", ifelse(!is.null(data),data$Vr,"..."))))  
   	
   	return(getTable(myTable))
   }),
   
   # Display results (table)
   output$Mod5Step1_summary_table2 <- renderUI({Mod5Step1_table2()})
   
   
) # End return
