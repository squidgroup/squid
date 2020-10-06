#Server functions for module 3 step 1
c(
    ######### Set variables #########    
      # Set hidden variables
       output$Mod3Step1_hidden <- renderUI({
          list(
            numericInput("Mod3Step1_Tmax", "", Modules_VAR$Tmax$max),
            numericInput("Mod3Step1_NI", "", 100),
            shinyMatrix::matrixInput("Mod3Step1_Vind", value = matrix(c(input$Mod3Step1_Vi,rep(0,(nb.IS*nb.IS)-1)),nb.IS), class = "numeric"),
            shinyMatrix::matrixInput("Mod3Step1_B", value = matrix(c(0,sqrt(input$Mod3Step1_Vbx),0,0),1), class = "numeric"),
            
            checkboxInput("Mod3Step1_X1_state", "", value = TRUE),
            
            checkboxInput("Mod3Step1_X1_sto_state", "", value = FALSE),
            checkboxInput("Mod3Step1_X1_lin_state", "", value = TRUE),
            
            checkboxInput("Mod3Step1_ST_ind", "", value = FALSE)
          )
        }),
 	outputOptions(output, "Mod3Step1_hidden", suspendWhenHidden = FALSE),

    ######### Run simulation #########
   	# Run simulation and return results
 	Mod3Step1_output <- reactive({
 	  if(input$Mod3Step1_Run == 0) # if Run button is pressed
 	    return(NULL)
 	  
 	  isolate({ 
 	    
 	    updateCheckboxInput(session, "isRunning", value = TRUE)
 	    
 	    # Call app main function
 	    data <- squid::squidR(input, module="Mod3Step1")  
 	    
 	    LMR      <- lme4::lmer(Phenotype ~ 0 + (1|Individual), data = data$sampled_data)
 	    RANDEF   <- as.data.frame(lme4::VarCorr(LMR))$vcov
 	    
 	    data$Vi            <- round(RANDEF[1],2)
 	    data$Vr            <- round(RANDEF[2],2)
 	    data$Vp            <- round(data$Vi + data$Vr,2)
 	    
 	    updateCheckboxInput(session, "isRunning", value = FALSE)
 	    
 	    return(data)
 	  })  
 	}),  
 	
 	output$Mod3Step1_previewPlot <- renderPlot({ 
 	  
 	  input$Mod3Step1_previewPlot_btn
 	  
 	  myInput <- list("Preview_Tmax"   = Modules_VAR$Tmax$max,
 	                  "Preview_NI"     = input$Mod3Step1_NI,
 	                  "Preview_Vhsi"   = input$Mod3Step1_Vhsi,
 	                  "Preview_NR"     = input$Mod3Step1_NR,
 	                  "Preview_ST_ind" = FALSE
 	  )
 	  # Call app main function
 	  data <- squid::squidR(myInput, module="Preview", plot=TRUE)
 	  print(data$plots$sampTime)
 	}),
 	
 	# Display results (table)
 	output$Mod3Step1_summary_table <- renderUI({ 
 	  
 	  data <- Mod3Step1_output()
 	  
 	  myTable <- data.frame("True"       = c(paste("Individual variance ($V_",NOT$devI,"$) =",input$Mod3Step1_Vi),
 	                                         paste("Measurement error variance ($V_",NOT$mError,"$) =",input$Mod3Step1_Ve),
 	                                         paste("Environmental variance ($V_",NOT$envEffect,"$) =",input$Mod3Step1_Vbx)),
 	                        "Estimated" = c(paste("Individual variance in sample ($V'_",NOT$devI,"$) = ",ifelse(!is.null(data),data$Vi,"...")),
 	                                        paste("Residual variance of sample ($V'_",NOT$residualUpper,"$) = ",ifelse(!is.null(data),data$Vr,"...")),
 	                                        "") 
 	  ) 
 	  getTable(myTable) 
 	}),
 	
 	output$Mod3Step1_Vi_proportion  <- renderText({paste0("(",round(input$Mod3Step1_Vi / (input$Mod3Step1_Vi + input$Mod3Step1_Vbx + input$Mod3Step1_Ve),2)*100,"%)")}),
 	output$Mod3Step1_Ve_proportion  <- renderText({paste0("(",round(input$Mod3Step1_Ve / (input$Mod3Step1_Vi + input$Mod3Step1_Vbx + input$Mod3Step1_Ve),2)*100,"%)")}),
 	output$Mod3Step1_Vbx_proportion <- renderText({paste0("(",round(input$Mod3Step1_Vbx / (input$Mod3Step1_Vi + input$Mod3Step1_Vbx + input$Mod3Step1_Ve),2)*100,"%)")})
 	
) # End return
