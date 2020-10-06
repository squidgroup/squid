#Server functions for module 3 step 2
c(
  
    ######### Set variables #########    
      # Set hidden variables (Tmax, Vi, ES_state, ES_sto_V and NR)
       output$Mod3Step2_hidden <- renderUI({
          list(
            numericInput("Mod3Step2_Tmax", "", Modules_VAR$Tmax$max),
            numericInput("Mod3Step2_NI", "", 100),
            getNumericInput("Mod3Step2_Tmax", Modules_VAR$Tmax, ""),
            shinyMatrix::matrixInput("Mod3Step2_Vind", value = matrix(c(input$Mod3Step2_Vi,rep(0,(nb.IS*nb.IS)-1)),nb.IS), class = "numeric"),
            shinyMatrix::matrixInput("Mod3Step2_B", value = matrix(c(0,sqrt(input$Mod3Step2_Vbx),0,0),1), class = "numeric"), 
            
            checkboxInput("Mod3Step2_X1_state", "", value = TRUE),
            
            checkboxInput("Mod3Step2_X1_sto_state", "", value = ifelse(input$Mod3Step2_X_select %in% c("sto","auto"),TRUE,FALSE)),
            checkboxInput("Mod3Step2_X1_sto_autocor_state", "", value = ifelse(input$Mod3Step2_X_select == "auto",TRUE,FALSE)),
            checkboxInput("Mod3Step2_X1_sto_shared", "", value = input$Mod3Step2_X_Shared),
            
            checkboxInput("Mod3Step2_X1_lin_state", "", value = ifelse(input$Mod3Step2_X_select == "lin",TRUE,FALSE)),
            checkboxInput("Mod3Step2_X1_lin_shared", "", value = input$Mod3Step2_X_Shared),
            
            checkboxInput("Mod3Step2_X1_cyc_state", "", value = ifelse(input$Mod3Step2_X_select == "cyc",TRUE,FALSE)),
            checkboxInput("Mod3Step2_X1_cyc_shared", "", value = input$Mod3Step2_X_Shared),
            
            checkboxInput("Mod3Step2_ST_ind", "", value = FALSE)
          )
        }),
 	outputOptions(output, "Mod3Step2_hidden", suspendWhenHidden = FALSE),
 	
 	  output$Mod3Step2_X1_plot <- renderPlot({squid::squidR(input, module="Mod3Step2" , X_previsualization="X1")}),
      
    ######### Run simulation #########
   	# Run simulation and return results
   	Mod3Step2_output <- reactive({
   	  if(input$Mod3Step2_Run == 0) # if Run button is pressed
   	    return(NULL)
   	  
   	  isolate({ 
   	    
   	    updateCheckboxInput(session, "isRunning", value = TRUE)
   	    
   	    # Call app main function
   	    data <- squid::squidR(input, module="Mod3Step2")  
   	    
   	    LMR      <- lme4::lmer(Phenotype ~ 0 + (1|Individual), data = data$sampled_data)
   	    RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
   	    
   	    data$Vi        <- round(RANDEF[1],2)
   	    data$Vr        <- round(RANDEF[2],2) 
   	    data$Vp        <- round(data$Vi + data$Vr,2)
   	    
   	    updateCheckboxInput(session, "isRunning", value = FALSE)
   	    
   	    return(data)
   	  })  
   	}),  
 	
 	output$Mod3Step2_previewPlot <- renderPlot({ 
 	  
 	  input$Mod3Step2_previewPlot_btn
 	  
 	  myInput <- list("Preview_Tmax"   = Modules_VAR$Tmax$max,
 	                  "Preview_NI"     = input$Mod3Step2_NI,
 	                  "Preview_Vhsi"   = input$Mod3Step2_Vhsi,
 	                  "Preview_NR"     = input$Mod3Step2_NR,
 	                  "Preview_ST_ind" = FALSE
 	                  )
 	  # Call app main function
 	  data <- squid::squidR(myInput, module="Preview", plot=TRUE)
 	  print(data$plots$sampTime)
 	}),
 	
   	# Display results (table)
   	output$Mod3Step2_summary_table <- renderUI({ 
   	  
   	  data <- Mod3Step2_output()
   	  
   	  myTable <- data.frame("True"       = c(paste("Individual variance ($V_",NOT$devI,"$) =",input$Mod3Step2_Vi),
   	                                         paste("Measurement error variance ($V_",NOT$mError,"$) =",input$Mod3Step2_Ve),
   	                                         paste("Environmental effect variance ($V_",NOT$envEffect,"$) =",input$Mod3Step2_Vbx)),
   	                        "Estimated" = c(paste("Individual variance in sample ($V'_",NOT$devI,"$) = ",ifelse(!is.null(data),data$Vi,"...")),
   	                                        paste("Residual variance of sample ($V'_",NOT$residualUpper,"$) = ",ifelse(!is.null(data),data$Vr,"...")),
   	                                        "")
   	  )  
   	  getTable(myTable)
   	}),
 	
   	output$Mod3Step2_Vi_proportion  <- renderText({paste0("(",round(input$Mod3Step2_Vi / (input$Mod3Step2_Vi + input$Mod3Step2_Vbx + input$Mod3Step2_Ve),2)*100,"%)")}),
   	output$Mod3Step2_Ve_proportion  <- renderText({paste0("(",round(input$Mod3Step2_Ve / (input$Mod3Step2_Vi + input$Mod3Step2_Vbx + input$Mod3Step2_Ve),2)*100,"%)")}),
   	output$Mod3Step2_Vbx_proportion <- renderText({paste0("(",round(input$Mod3Step2_Vbx / (input$Mod3Step2_Vi + input$Mod3Step2_Vbx + input$Mod3Step2_Ve),2)*100,"%)")}),

    ######### Manage errors #########
     	# display error message
     	observe({
     	  if(
     	     !testInput(input$Mod3Step2_X1_sto_V, FullModel_VAR$stoV, FALSE, FALSE) ||
     	     !testInput(input$Mod3Step2_X1_sto_corr, FullModel_VAR$stoCorr, FALSE, FALSE)){
     	  	disableActionButton("Mod3Step2_Run", session, "true")
     	  }else{
     	  	disableActionButton("Mod3Step2_Run", session, "false")
     	  }
     	}),
 	    output$Mod3Step2_error_sto_V     <- renderUI({testInput(input$Mod3Step2_X1_sto_V, FullModel_VAR$stoV, FALSE, TRUE)}),
 	    output$Mod3Step2_error_sto_corr  <- renderUI({testInput(input$Mod3Step2_X1_sto_corr, FullModel_VAR$stoCorr, FALSE, TRUE)})

) # End return